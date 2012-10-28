$(function () {
  var logger = $("#log");

  var createEnv = function(parms, args, outer) {
    parms = parms || [];
    args = args || [];
    outer = outer || null;

    var ret = {};
    _.zip(parms,args).map(function(i) {
      ret[i[0]] = i[1];
    });
    ret["outer"] = outer;
    ret.find = function(varname) {
      return ret[varname] ? ret : outer.find(varname);
    };
    return ret;
  };

  var eval = (function() {
    var Global_env = (function(env) {
      var funcs = {
        '+': function(i,j) {return i+j},
        '-': function(i,j) {return i-j},
        '*': function(i,j) {return i*j},
        '/': function(i,j) {return i/j},
        '>': function(i,j) {return i>j},
        '<': function(i,j) {return i<j},
        '>=': function(i,j) {return i>=j},
        '<=': function(i,j) {return i<=j},
        '=': function(i,j) {return i===j},
        'not': function(i) {return !i},
        'append': function(i,j) {return i.concat(j)},
        'equal?': function(i,j) {return i===j},
        'eq?': function(i,j) {return i===j},
        'length': function(i) {return i.length},
        'cons': function(i,j) {return [i].concat(j)},
        'car': function(i) {return i[0]},
        'cdr': function(i) {return _.rest(i)},
        'list': function() {return _.toArray(arguments)},
        'null?': function() {return x === []},
        'symbol?': function() {return typeof x === 'string'},
      };

      for (i in ['E','LN2','LN10','LOG2E','LOG10E','PI','SQRT1_2','SQRT2',
          'abs','acos','asin','atan','atan2','ceil','cos','exp','floor',
          'log','max','min','pow','random','round','sin','sqrt','tan' ]) {
        funcs[i] = Math[i];
      }

      for (i in funcs) {
        env[i] = funcs[i];
      }

      return env;
    })(createEnv());

    return function(x, env) {
      if (_.isUndefined(env)) {
        env = Global_env;
      }

      if (typeof x === 'string') {
        return env.find(x)[x];
      } else if (!_.isArray(x)) {
        return x;
      } else if (x[0] === 'quote') {
        return x[1];
      } else if (x[0] === 'if') {
        var test = x[1];
        var conseq = x[2];
        var alt = x[3];
        return eval(eval(test, env) ? conseq : alt, env);
      } else if (x[0] === 'set!') {
        var varname = x[1];
        var exp = x[2];
        env.find(varname)[varname] = eval(exp,env);
      } else if (x[0] === 'define') {
        var varname = x[1];
        var exp = x[2];
        env[varname] = eval(exp,env);
      } else if (x[0] === 'lambda') {
        var varname = x[1];
        var exp = x[2];
        return function() {return eval(exp,createEnv(varname, _.toArray(arguments), env));};
      } else if (x[0] === 'begin') {
        var ret;
        for (var i=0 ; i<x.length ; i++) {
          ret = eval(exp,env);
        }
        return ret;
      } else {
        var exps = x.map(function(i) {return eval(i,env);});
        prc = exps.shift();
        return prc.apply(null,exps);
      }
      return exp;
    };
  })();

  var read_from = function(tokens){
    if (tokens.length === 0) {
      throw {name: "SyntaxError", message: "unexpected EOF while reading"};
    }
    token = tokens.shift();
    if ('(' === token) {
      var ret = [];
      while (tokens[0] !== ')') {
        ret.push(read_from(tokens));
      }
      tokens.shift();
      return ret;
    } else if (')' === token) {
      throw {name: "SyntaxError", message: "unexpected )"};
    } else {
      return atom(token);
    }
  };

  var tokenize = function(s) {
    return s.replace(/\(/g, ' ( ').replace(/\)/g, ' ) ')
      .split(/\s+/).filter(function(i){return i!==''});
  };

  var read = function(s) {
    return read_from(tokenize(s));
  };

  var parse = function(s) {
    return read(s);
  }

  var atom = function(token) {
    return _.isFinite(Number(token)) ? Number(token) : token;
  }

  var to_string = function(exp) {
    return _.isArray(exp) ? '('+ exp.map(function(i){return to_string(i)}).join(' ') +')' : (""+exp);
  };

  $("#code").keydown(function(e) {
    if (e.keyCode == 13) {
      var input = $("#code").val();

      logger.prepend("INPUT> " + input + "<br/>");
      if (input !== "") {
        try {
          var val = eval(parse(input));
          logger.prepend("-> " + to_string(val) + "<br/>");
        } catch (e) {
          logger.prepend(e.name + ': ' + e.message);
        }
      }
      $("#code").attr("value","");
    }
    return true;
  });

});

