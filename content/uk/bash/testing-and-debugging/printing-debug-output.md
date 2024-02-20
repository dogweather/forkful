---
date: 2024-01-20 17:52:08.541804-07:00
description: "\u0412\u0438\u0432\u0435\u0434\u0435\u043D\u043D\u044F \u043D\u0430\u043B\
  \u0430\u0433\u043E\u0434\u0436\u0443\u0432\u0430\u043B\u044C\u043D\u043E\u0457 \u0456\
  \u043D\u0444\u043E\u0440\u043C\u0430\u0446\u0456\u0457 \u2013 \u0446\u0435 \u0441\
  \u043F\u043E\u0441\u0456\u0431 \u0431\u0430\u0447\u0438\u0442\u0438, \u0449\u043E\
  \ \u0432\u0456\u0434\u0431\u0443\u0432\u0430\u0454\u0442\u044C\u0441\u044F \u0443\
  \ \u0432\u0430\u0448\u043E\u043C\u0443 \u043A\u043E\u0434\u0456 \u043F\u0456\u0434\
  \ \u0447\u0430\u0441 \u0432\u0438\u043A\u043E\u043D\u0430\u043D\u043D\u044F. \u041F\
  \u0440\u043E\u0433\u0440\u0430\u043C\u0456\u0441\u0442\u0438 \u0440\u043E\u0431\u043B\
  \u044F\u0442\u044C \u0446\u0435, \u0449\u043E\u0431 \u0440\u043E\u0437\u0443\u043C\
  \u0456\u0442\u0438 \u043F\u0440\u043E\u0446\u0435\u0441\u0438 \u0432\u2026"
lastmod: 2024-02-19 22:05:08.618156
model: gpt-4-1106-preview
summary: "\u0412\u0438\u0432\u0435\u0434\u0435\u043D\u043D\u044F \u043D\u0430\u043B\
  \u0430\u0433\u043E\u0434\u0436\u0443\u0432\u0430\u043B\u044C\u043D\u043E\u0457 \u0456\
  \u043D\u0444\u043E\u0440\u043C\u0430\u0446\u0456\u0457 \u2013 \u0446\u0435 \u0441\
  \u043F\u043E\u0441\u0456\u0431 \u0431\u0430\u0447\u0438\u0442\u0438, \u0449\u043E\
  \ \u0432\u0456\u0434\u0431\u0443\u0432\u0430\u0454\u0442\u044C\u0441\u044F \u0443\
  \ \u0432\u0430\u0448\u043E\u043C\u0443 \u043A\u043E\u0434\u0456 \u043F\u0456\u0434\
  \ \u0447\u0430\u0441 \u0432\u0438\u043A\u043E\u043D\u0430\u043D\u043D\u044F. \u041F\
  \u0440\u043E\u0433\u0440\u0430\u043C\u0456\u0441\u0442\u0438 \u0440\u043E\u0431\u043B\
  \u044F\u0442\u044C \u0446\u0435, \u0449\u043E\u0431 \u0440\u043E\u0437\u0443\u043C\
  \u0456\u0442\u0438 \u043F\u0440\u043E\u0446\u0435\u0441\u0438 \u0432\u2026"
title: "\u0412\u0438\u0432\u0435\u0434\u0435\u043D\u043D\u044F \u043D\u0430\u043B\u0430\
  \u0433\u043E\u0434\u0436\u0443\u0432\u0430\u043B\u044C\u043D\u043E\u0457 \u0456\u043D\
  \u0444\u043E\u0440\u043C\u0430\u0446\u0456\u0457"
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
