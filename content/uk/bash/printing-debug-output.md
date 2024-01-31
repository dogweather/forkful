---
title:                "Виведення налагоджувальної інформації"
date:                  2024-01-20T17:52:08.541804-07:00
model:                 gpt-4-1106-preview
simple_title:         "Виведення налагоджувальної інформації"

category:             "Bash"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/bash/printing-debug-output.md"
---

{{< edit_this_page >}}

## Що це таке & Навіщо?
Виведення налагоджувальної інформації – це спосіб бачити, що відбувається у вашому коді під час виконання. Програмісти роблять це, щоб розуміти процеси в системі і швидко знаходити помилки.

## Як це робити:
```Bash
# Echo command for simple debugging
echo "Debug: The value of variable 'x' is $x."

# Redirecting standard and error output to a file
ls -l /nonexistent/directory 2>&1 | tee debug_output.txt

# Using set -x to trace what gets executed
set -x
cp /source/file.txt /destination/
set +x
```
Output Sample:
```
Debug: The value of variable 'x' is 7.
ls: cannot access '/nonexistent/directory': No such file or directory
+ cp /source/file.txt /destination/
```

## Поглиблене вивчення
Historically, Bash didn't have advanced debugging tools. Simple `echo` statements were a primary way to output variable values and flow of control. As Bash developed, more options emerged. The `set -x` command outputs each command and its arguments as the script executes, which can be invaluable. 

There are alternatives to Bash for debugging, such as using a formal debugger like `bashdb` or writing scripts in languages with better debugging support.

For implementation, note that using `echo` can interfere with actual output, so consider directing debug output to files or using `stderr`. The `tee` command is handy to watch the output in real-time while preserving it in a file. 

## Дивись також
- Bash Hackers Wiki on Debugging (https://wiki.bash-hackers.org/scripting/debuggingtips)
- Advanced Bash-Scripting Guide: Chapter 37. Debugging (https://tldp.org/LDP/abs/html/debugging.html)
- `bashdb`, the Bash Debugger (https://bashdb.sourceforge.io/)
