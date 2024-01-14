---
title:                "Bash: 명령줄 인수 읽기"
programming_language: "Bash"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/bash/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 왜

리눅스와 유닉스 시스템에서 Bash 프로그래밍을 하기 위해서는 커맨드 라인 인자(argument)를 읽는 방법을 알아야 합니다. 이 기술은 프로그램을 실행할 때 사용자로부터 입력을 받는 방법을 제공하며, 실행시에 다른 옵션을 주거나 파라미터를 전달할 수 있도록 해줍니다. 커맨드 라인 인자를 읽는 기술은 여러분이 더 유연하고 고급 사용자들이 자주 사용하는 기능을 사용할 수 있도록 도와줍니다.

## 방법

커맨드 라인 인자를 읽는 방법은 매우 간단합니다. 이를 위해서는 프로그램에 입력되는 인자를 저장해주는 변수를 지정해주면 됩니다. 예를 들어, 스크립트 파일이 myscript.sh라고 한다면 아래 코드를 추가하여 커맨드 라인 인자를 저장할 수 있습니다. 

```Bash
args=("$@")
```

여러분은 이 변수를 이용하여 원하는 대로 커맨드 라인 인자에 접근할 수 있습니다. 아래는 간단한 코드 예제와 실행 결과입니다.

```Bash
$ bash myscript.sh 1 2 3
$ echo ${args[0]}
1 # 첫번째 인자
$ echo ${args[2]}
3 # 세번째 인자
```

## 깊게 들어가기

왜 우리 프로그램에는 커맨드 라인 인자를 읽어야 할까요? 이는 여러분의 프로그램이 다양한 상황에서 유연하게 실행되고 다양한 설정을 할 수 있도록 하기 위함입니다. 예를 들어, 여러분이 자신의 스크립트를 실행할 때 다른 옵션들을 추가해서 실행하고 싶다고 가정해봅시다. 커맨드 라인 인자를 사용하면 이를 간단하게 해결할 수 있습니다.

또한, 여러분이 자주 사용하는 명령어를 스크립트로 만들 때 커맨드 라인 인자를 사용하면 실행할 때마다 파라미터를 지정해주지 않아도 됩니다. 이는 여러분의 시스템에 더 많은 손실을 일으키지 않고 자신만의 강력한 도구를 만들 수 있도록 해줍니다.

## 이어보기

만약 여러분이 Bash 프로그래밍과 커맨드 라인 인자에 대해 더 자세히 알고 싶다면 아래 링크들을 참고해보세요.

[The Bash Guide](https://wiki.bash-hackers.org/scripting)

[Bash Shell Basics](https://www.howtogeek.com/140679/basics-of-bash-shell-scripting-for-beginners/)

[Argument Parsing in Bash](https://stackoverflow.com/questions/192249/how-do-i-parse-command-line-arguments-in-bash)