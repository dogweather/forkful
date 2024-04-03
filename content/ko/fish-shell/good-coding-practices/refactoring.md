---
date: 2024-01-26 01:18:18.277430-07:00
description: "\uB9AC\uD329\uD1A0\uB9C1\uC740 \uAE30\uC874 \uCF54\uB4DC\uC758 \uC678\
  \uBD80 \uD589\uB3D9\uC744 \uBCC0\uACBD\uD558\uC9C0 \uC54A\uACE0 \uAD6C\uC870\uB97C\
  \ \uC7AC\uC870\uC815\uD558\uC5EC \uBE44\uAE30\uB2A5\uC801 \uC18D\uC131\uC744 \uAC1C\
  \uC120\uD558\uB294 \uACFC\uC815\uC785\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\
  \uB4E4\uC740 \uCF54\uB4DC\uB97C \uB354 \uC77D\uAE30 \uC27D\uAC8C, \uBCF5\uC7A1\uC131\
  \uC744 \uC904\uC774\uACE0, \uC720\uC9C0\uBCF4\uC218\uC131\uC744 \uB192\uC774\uBA70\
  , \uAE38\uC744 \uB098\uC544\uAC00\uBA74\uC11C \uD655\uC7A5\uD558\uAC70\uB098 \uC218\
  \uC815\uD558\uAE30 \uC27D\uAC8C \uB9CC\uB4E4\uAE30 \uC704\uD574 \uC774 \uC791\uC5C5\
  \uC744 \uD569\uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:55.868234-06:00'
model: gpt-4-0125-preview
summary: "\uB9AC\uD329\uD1A0\uB9C1\uC740 \uAE30\uC874 \uCF54\uB4DC\uC758 \uC678\uBD80\
  \ \uD589\uB3D9\uC744 \uBCC0\uACBD\uD558\uC9C0 \uC54A\uACE0 \uAD6C\uC870\uB97C \uC7AC\
  \uC870\uC815\uD558\uC5EC \uBE44\uAE30\uB2A5\uC801 \uC18D\uC131\uC744 \uAC1C\uC120\
  \uD558\uB294 \uACFC\uC815\uC785\uB2C8\uB2E4."
title: "\uB9AC\uD329\uD1A0\uB9C1"
weight: 19
---

## 무엇 & 왜?
리팩토링은 기존 코드의 외부 행동을 변경하지 않고 구조를 재조정하여 비기능적 속성을 개선하는 과정입니다. 프로그래머들은 코드를 더 읽기 쉽게, 복잡성을 줄이고, 유지보수성을 높이며, 길을 나아가면서 확장하거나 수정하기 쉽게 만들기 위해 이 작업을 합니다.

## 방법:
시간이 지나면서 상당히 커져버린 스크립트가 있다고 상상해 보세요. 처음에는 단순했지만, 이제 그것은 로직의 문어발처럼 퍼져나간 짐승이 되었습니다. 여기 리팩토링을 통해 함수를 더 읽기 쉽고 효율적으로 만든 한 입 크기의 예시가 있습니다:

리팩토링 전:
```fish
function old_and_clunky
    set color (cat ~/.config/fish/color_theme)
    if test "$color" = 'blue'
        echo 'Blue theme set!'
    else if test "$color" = 'red'
        echo 'Red theme set!'
    else
        echo 'Default theme set!'
    end
end
```

리팩토링 후:
```fish
function set_theme_color
    set theme_color (cat ~/.config/fish/color_theme)
    switch $theme_color
        case blue
            echo 'Blue theme set!'
        case red
            echo 'Red theme set!'
        default
            echo 'Default theme set!'
    end
end
```
이 리팩토링은 함수의 이름을 그 목적을 더 잘 설명하는 것으로 개선했고, if-else 체인을 깔끔한 `switch` 문으로 대체했습니다.

샘플 출력:
```
Blue theme set!
```

## 심층 분석
리팩토링은 마틴 파울러의 기념비적인 책 "리팩토링: 기존 코드의 설계 개선"에서 처음으로 자세히 설명되었습니다. 이 책은 새로운 기능을 작성하지 않고 코드를 개선하는 구조적 접근 방법을 제시했습니다. 그 이후로 많은 리팩토링 기법이 소개되었으며, 이 개념은 현대 소프트웨어 개발의 기본적인 부분이 되었습니다.

Fish Shell 환경에서의 리팩토링은 그것의 특수한 구문과 명령줄 성격 때문에 다른 프로그래밍 맥락과는 약간 다르게 보일 수 있습니다. Fish에서 스크립트를 리팩토링하는 대안은 다른 셸 언어로 포팅하거나 보다 고급 스크립트 관리를 위해 외부 도구를 사용하는 것일 수 있습니다. 그러나 네이티브 Fish 구문을 유지하는 것은 종종 셸의 기능과 더 잘 통합되고 전반적으로 더 효율적인 경험을 의미합니다.

Fish Shell에서 리팩토링할 때, 다른 언어에서 흔한 광범위한 클래스나 모듈과 달리 대부분 함수와 명령어를 다루고 있습니다. 이러한 세밀함으로 인해 리팩토링 작업이 더 즉각적이고 직접적인 과정이 될 수 있지만, 명확하고 간결하며 유지보수가 용이한 코드의 중요성도 강조합니다.

## 참고
- 마틴 파울러의 리팩토링 웹사이트: [https://refactoring.com/](https://refactoring.com/)
- 공식 Fish Shell 문서: [https://fishshell.com/docs/current/index.html](https://fishshell.com/docs/current/index.html)
