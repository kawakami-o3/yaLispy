function increment(num: number) {
	return num+1;
}

const buf = new Uint8Array(1024);
const n = <number>await Deno.stdin.read(buf);
//const num = Number((new TextDecoder().decode(buf.subarray(0, n)).trim()));
//const num = parseInt((new TextDecoder().decode(buf.subarray(0, n)).trim()), 10);
const num = parseFloat((new TextDecoder().decode(buf.subarray(0, n)).trim()));
//const title = Number(buf.trim());
//console.log(increment(n));
console.log(increment(num));


