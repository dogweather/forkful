---
title:    "Bash: 컴퓨터 프로그래밍에서의 명령줄 인수 읽기"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/bash/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 왜

Bash 프로그래밍은 매우 강력하고 유용하며, 커맨드 라인 인자를 읽는 것은 프로그램을 더 유연하고 사용자 정의할 수 있게 만들어줍니다. 따라서 Bash 스크립트를 작성하는 모든 개발자는 커맨드 라인 인자를 어떻게 읽을 수 있는지 학습해야 합니다.

## 어떻게

커맨드 라인 인자를 읽는 가장 간단한 방법은 "read" 명령어를 사용하는 것입니다. 이 명령어는 사용자로부터 입력을 받아 변수에 저장할 수 있도록 해줍니다. 예를 들어, 아래의 코드는 사용자로부터 입력을 받아 "input" 변수에 저장합니다.

```Bash
echo "입력을 입력하세요: "
read input
```

위의 예시에서, 사용자가 입력을 했을 때, 그 값은 "input" 변수에 저장되었고 이제 그 값을 사용할 수 있게 됩니다. 또 다른 방법으로는, "getopts"를 사용하는 것입니다. 이 명령어는 스크립트에 옵션을 넣어 인자를 읽을 수 있게 해줍니다. 예를 들어, 아래의 코드는 "a"와 "b"라는 두 가지 옵션을 가지고 있으며, 각각의 옵션에 해당하는 메시지를 출력합니다.

```Bash
while getopts "ab" option; do
case $option in
a) echo "Option a가 선택되었습니다." ;;
b) echo "Option b가 선택되었습니다." ;;
esac
done
```

위의 예시에서는 "bash script.sh -a"를 실행했을 때 "Option a가 선택되었습니다."라는 메시지가 출력됩니다. "bash script.sh -b"를 실행하면 "Option b가 선택되었습니다."라는 메시지가 출력됩니다. "a"와 "b" 옵션을 동시에 사용하고 싶다면 "getopts" 옵션 뒤에 ":"를 추가하면 됩니다. 이렇게 하면 다양한 옵션을 가진 스크립트를 작성할 수 있게 됩니다.

## 깊이 파고들기

커맨드 라인 인자를 읽는 가장 일반적인 방법은 "getopts" 명령어를 사용하는 것입니다. 이 명령어가 기본적으로 어떻게 작동하는지, 그리고 어떻게 변수와 함께 사용하는지 알아보겠습니다.

"getopts" 명령어는 스크립트의 인자를 변수에 저장합니다. 첫 번째 인자를 $1, 두 번째 인자를 $2와 같이 순서대로 저장하며, 인자의 개수는 "$#" 변수에 저장됩니다. 그리고 "getopts"는 while 루프 안에서 사용되며 입력되는 옵션을 변수에 저장합니다. 해당 옵션이 존재하지 않으면 "?"를 리턴하며, 옵션이 ":"으로 시작하면 그 다음 옵션이 그 변수에 저장됩니다. 마지막으로 "shift" 명령어를 사용해서 처리된 인자를 지워주며 다음 인자로 이동합니다.

## 더 알아보기

"getopts" 명령어 외에도 커맨드 라인 인자를 읽는 다른 방법들이 존재합니다. 이를 더 자세히 알고 싶다면 아래의 링크들을 참고해보세요.

[Advanced Bash-Scripting Guide - Arguments](https://tldp.org/LDP/abs/html/explorequickref.html#COMMANDOPLINEARGS)
[How to Read