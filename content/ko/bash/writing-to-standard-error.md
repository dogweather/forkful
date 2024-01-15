---
title:                "표준 오류에 작성하기"
html_title:           "Bash: 표준 오류에 작성하기"
simple_title:         "표준 오류에 작성하기"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/bash/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## 왜

Bash 프로그래밍을 할 때, 표준 오류에 쓰는 것은 중요합니다. 이것은 오류를 감지하고 디버깅하는 데 매우 유용합니다.
이외에도 표준 출력에 쓰는 것보다 오류 메시지가 더 직관적이기 때문에 사용자에게 더욱 이해하기 쉽습니다.

## 방법

일반적으로 오류 메시지를 작성하는 방법은 ```echo```를 사용하는 것입니다. 다음과 같이 코드를 작성하면 됩니다.
```
echo "에러 메시지" 1>&2
```
여기서 ```1>&2```는 표준 오류에 쓰기 위한 코드입니다. ```1```은 표준 출력을 의미하고, ```2```는 표준 오류를 의미합니다.

예를 들어, 사용자의 입력이 올바르지 않은 경우 "잘못된 입력입니다."라는 오류 메시지를 출력하고 싶다면 아래와 같이 작성할 수 있습니다.
```
echo "잘못된 입력입니다." 1>&2
```

## 깊은 이해

일반적으로 프로그램은 두 가지 출력 스트림을 가지고 있습니다. 표준 출력은 사용자에게 결과를 보여주기 위해 사용되는 스트림이며, 표준 오류는 오류 메시지를 출력하기 위해 사용되는 스트림입니다.

따라서 오류 메시지를 표준 오류에 쓰는 것은 사용자에게 더 많은 정보를 제공하고 디버깅을 더 쉽게 만듭니다. 이는 오류를 신속하게 발견하고 해결하는 데 중요합니다.

## 참고하기

- [Bash 표준 출력 및 오류 출력 제어](https://www.gnu.org/software/bash/manual/html_node/Redirections.html)
- [Bash 스크립트에서 오류 처리하기](https://www.tldp.org/LDP/abs/html/exit-status.html)