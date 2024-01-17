---
title:                "명령 줄 인수 읽기"
html_title:           "Bash: 명령 줄 인수 읽기"
simple_title:         "명령 줄 인수 읽기"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/bash/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 무엇인가 & 왜?:
커맨드라인 인자를 읽는 것은 프로그래머가 사용자로부터 입력받은 데이터를 읽어오는 것을 말합니다. 이를 통해 우리는 프로그램을 실행할때 다양한 입력 값을 전달할 수 있습니다. 그리고 이러한 기능은 사용자의 요구에 맞는 커스터마이징된 프로그램을 만드는 데에 도움이 됩니다.

## 하우 투:
```Bash
echo "Hello $1!"
```
위 코드는 "Hello" 다음에 오는 첫 번째 인자를 받아서 출력하는 예제입니다. 예를 들어, "bash example.sh world"를 실행하면 "Hello world!"라는 결과가 나옵니다.

```Bash
echo "The sum of $1 and $2 is $(($1+$2))"
```
이 코드는 첫 번째와 두 번째 인자를 받아서 그 값을 더한 후 출력하는 예제입니다. 예를 들어, "bash example.sh 3 5"를 실행하면 "The sum of 3 and 5 is 8"라는 결과가 나옵니다.

## 깊이 들어가보기:
커맨드라인 인자를 읽는 것은 일반적으로 C언어나 자바를 비롯한 다양한 프로그래밍 언어에서 가능합니다. 이는 입력값을 전달하는 간편한 방법이기 때문에 매우 흔한 기능입니다. 그러나 이 방법보다 더 복잡한 입력 값 처리가 필요한 경우, 사용자가 직접 입력하는 대화형 명령어를 받는 전용 라이브러리를 사용하는 것도 가능합니다.

## 참고 자료:
- [Bash command line arguments](https://www.tutorialspoint.com/unix_commands/bash.htm)
- [Reading command line arguments in Bash](https://www.gnu.org/software/bash/manual/html_node/Bash-Builtins.html)
- [Command line arguments in Bash scripting](https://www.shell-tips.com/bash/command-line-arguments-in-bash-scripting/)
- [Bash scripting cheatsheet](https://devhints.io/bash)