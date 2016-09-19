
type response =
  | Yes
  | No
  | PrettyMuch;
let isSafeToLaunchRocket = PrettyMuch;
let message = switch isSafeToLaunchRocket {
  | No => "Check Integrity."
  | Yes => "All Systems Go."
};

print_string message;
print_string "\n";
