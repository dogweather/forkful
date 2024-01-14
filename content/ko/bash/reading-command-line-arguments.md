---
title:    "Bash: 커맨드 라인 인수 읽기"
keywords: ["Bash"]
---

{{< edit_this_page >}}

## 왜
커맨드 라인 인자를 읽어보는 것이 왜 중요한지 궁금하지 않으신가요? 이 블로그 포스트를 통해 Bash 프로그래밍의 기초를 배우고 커맨드 라인 인자를 사용하는 방법을 배울 수 있습니다.

## 방법
우선 Bash 스크립트를 작성할 때 ```$1```, ```$2```, ```$@```와 같은 커맨드 라인 인자를 사용합니다. 매우 간단한 예제로써, 다음과 같은 코드를 작성해봅시다:

```Bash
#!/bin/bash
echo "첫 번째 인자: $1"
echo "두 번째 인자: $2"
```

위 코드를 실행하면 스크립트의 첫 번째 인자와 두 번째 인자가 출력됩니다. 예를 들어, ```bash script.sh dog cat```과 같이 스크립트를 실행하면 "첫 번째 인자: dog", "두 번째 인자: cat"가 출력됩니다.

커맨드 라인 인자를 배열 형태로도 사용할 수 있습니다. 아래 코드를 참고해주세요:

```Bash
#!/bin/bash
args=("$@")
echo "첫 번째 인자: ${args[0]}"
echo "두 번째 인자: ${args[1]}"
```

이번에는 ```bash script.sh dog cat bird```처럼 인자를 세 개 주고 스크립트를 실행하면 "첫 번째 인자: dog", "두 번째 인자: cat", "세 번째 인자: bird"가 출력됩니다.

## 딥 다이브
이제 커맨드 라인 인자를 읽는 방법을 더 자세히 알아봅시다. 커맨드 라인 인자는 스크립트에서 ```$1```, ```$2```와 같이 사용되기 때문에 순서가 중요합니다. 또한 ```$@```는 모든 인자를 나타내기 때문에 반복문과 함께 사용하면 편리합니다.

커맨드 라인 인자를 사용할 때 유저가 입력한 인자의 수를 체크하는 것도 중요합니다. 이때 ```$#```을 사용하면 쉽게 체크할 수 있습니다. 예를 들어, ```if [ $# -lt 2 ]```와 같이 사용하여 인자가 두 개 미만이면 에러 메시지를 출력할 수 있습니다.

## See Also
- [Bash Scripting Tutorial](https://ryanstutorials.net/bash-scripting-tutorial/)
- [Bash Guide for Beginners](https://www.tldp.org/LDP/Bash-Beginners-Guide/html/)
- [Linuxize: Working with Bash Script Arguments](https://linuxize.com/post/bash-script-arguments/)

커맨드 라인 인자에 대해 더 자세히 알아보려면 위의 링크들을 참고해주세요. 커맨드 라인 인자는 Bash 프로그래밍에서 기초적인 요소이며, 잘 활용하면 보다 유연하고 사용하기 쉬운 스크립트를 작성할 수 있습니다. 감사합니다!