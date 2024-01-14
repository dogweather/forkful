---
title:    "Fish Shell: 미래나 과거의 날짜 계산하기"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

## 왜

날짜를 미래나 과거로 계산하는 것에 참여하는 이유는 무엇일까요? 단 1-2 문장으로 설명하겠습니다.

날짜 계산은 많은 프로그래밍 언어에서 사용할 수 있는 유용한 기능 중 하나입니다. 예를 들어, 이를 통해 예약 시스템이나 기간 제한이 있는 작업을 할 수 있습니다.

## 어떻게

우선 Fish Shell을 열고, 다음과 같은 명령어를 입력해 보세요.

```Fish Shell
set yesterday (date -d "yesterday" +"%Y-%m-%d")
```

이 명령어는 어제의 날짜를 "YYYY-MM-DD" 형식으로 출력해 줍니다.

이제 오늘로부터 일주일 전의 날짜를 계산해 보겠습니다.

```Fish Shell
set last_week (date -d "-1 week" +"%Y-%m-%d")
```

이 명령어는 현재 날짜로부터 일주일 전의 날짜를 "YYYY-MM-DD" 형식으로 출력해 줍니다.

그리고 필요에 따라 더 많은 예제를 실험해 보세요. Fish Shell에서는 다양한 날짜 계산 함수를 사용할 수 있습니다. 또한, 날짜를 문자열이 아닌 다른 형식으로 출력하고자 할 때에도 유용하게 사용할 수 있습니다.

## 더 깊이

날짜 계산에는 많은 방법이 있습니다. 다양한 명령어와 옵션을 사용해 원하는 날짜를 출력할 수 있습니다. 또한, 날짜 계산에 유용한 함수들도 많이 존재합니다. Fish Shell 공식 문서를 참고해 보면 이러한 함수와 명령어를 더 자세히 알아볼 수 있습니다.

## 참고

[Fish Shell 공식 문서](https://fishshell.com/docs/current/index.html)

[Fish Shell 위키북스](https://wiki.kldp.org/HOWTO/html/Adv-Bash-Scr-HOWTO/)

[Fish Shell 학습 리소스](https://ryanstutorials.net/bash-scripting-tutorial/bash-if-statements.php)