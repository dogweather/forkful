---
title:                "명령줄 인수 읽기"
html_title:           "Arduino: 명령줄 인수 읽기"
simple_title:         "명령줄 인수 읽기"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/bash/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 무엇이며 왜 사용하는가?

명령 줄 인자 읽기는 스크립트 실행 시 사용자로부터 추가적인 정보를 전달하는 방법입니다. 이를 통해 프로그램의 동작을 유동적으로 조절할 수 있습니다.

## 어떻게 사용하는가?

아래는 Bash에서 명령 줄 인자를 읽는 방법에 대한 예제 코드입니다.

```Bash
#!/bin/bash
echo "첫 번째 인자: $1"
echo "두 번째 인자: $2"
```
이 스크립트를 실행하면 첫 번째와 두 번째 입력 인자가 각각 출력됩니다. 예를 들어, `./myscript.sh Hello World` 명령어를 실행하면 아래와 같은 출력을 볼 수 있습니다.

```Bash
첫 번째 인자: Hello
두 번째 인자: World
```
## 깊게 알아보기

**역사적 맥락:** Bash는 1989년, Brian Fox에 의해 만들어졌습니다. 이후 번들이네 와일드카드, 파일 완성 등 다양한 기능이 추가되었습니다.

**대안:** Bash 외에도 Python, Ruby, Perl 등 다양한 스크립팅 언어에서 명령 줄 인자를 읽을 수 있습니다. 각 언어마다 사용자 친화적인 인터페이스를 제공하므로 개발자의 선호도에 따라 선택할 수 있습니다.

**구현 세부사항:** Bash는 인자를 배열로 처리하여 접근을 용이하게 합니다. `$0`은 스크립트 이름, `$1`부터 `$n`까지는 각각의 인자를 나타냅니다. 또한, `$*` 혹은 `$@`를 사용하면 모든 인자를 한 번에 확인할 수 있습니다.

## 참고자료

* Bash Reference Manual: [https://www.gnu.org/software/bash/manual/bash.html](https://www.gnu.org/software/bash/manual/bash.html)
* Linux Command Line Arguments Tutorial: [https://ryanstutorials.net/bash-scripting-tutorial/bash-input.php](https://ryanstutorials.net/bash-scripting-tutorial/bash-input.php)