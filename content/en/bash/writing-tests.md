---
title:                "Writing tests"
date:                  2024-01-19
simple_title:         "Writing tests"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/bash/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why?

Writing tests verifies that code behaves as expected. Programmers test to catch bugs early, ensure reliability, and facilitate updates safely.

## How to:

Bash doesn't have a built-in testing framework, but you can use simple test commands and assertions. Let's write a function and test it using `test` or `[ ]`.

Bash function to test:
```Bash
is_number() {
  [[ $1 =~ ^[0-9]+$ ]] && return 0 || return 1
}
```

Test case:
```Bash
test_is_number() {
  is_number 42 && echo "Pass: 42 is a number" || echo "Fail: 42 is not a number"
  is_number abc && echo "Pass: abc is a number" || echo "Fail: abc is not a number"
}

test_is_number
```

Sample output:
```
Pass: 42 is a number
Fail: abc is not a number
```

## Deep Dive

Bash testing has evolved. Initially, one would manually check script outputs. Then came the `test` command in the 1970s, which let scripts self-check conditions. Alternatives like `Bats`, an actual Bash testing framework, offer more features but require external installation. Combining `Bats` with Continuous Integration (CI) tools like Jenkins or GitHub Actions leads to more robust testing pipelines. While implementing, remember Bash is less granular than other languages' testing frameworks; use `-eq` for number comparison, `-z` to check if a string is null, and `[[ ]]` for advanced features like pattern matching.

## See Also

- [Bats: Bash Automated Testing System](https://github.com/bats-core/bats-core)
- [Advanced Bash-Scripting Guide: Testing and Branching](https://tldp.org/LDP/abs/html/testbranch.html)
- [ShellCheck: A static analysis tool for shell scripts](https://www.shellcheck.net/)
