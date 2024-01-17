---
title:                "랜덤 숫자 생성"
html_title:           "Javascript: 랜덤 숫자 생성"
simple_title:         "랜덤 숫자 생성"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/javascript/generating-random-numbers.md"
---

{{< edit_this_page >}}

# 무엇이고 왜 생성 하는가?
랜덤한 숫자 생성이란 무엇인지 설명하고, 프로그래머들이 이 작업을 왜 하는지에 대해 두세 문장으로 알려드리겠습니다.

# 방법:
```Javascript
// 간단한 숫자 생성 예시
console.log(Math.random()); // 0과 1 사이의 무작위 값 출력
console.log(Math.floor(Math.random() * 10) + 1); // 1부터 10까지의 임의의 정수 출력
```

# 자세히 알아보기:
1. 역사적 배경: 난수 생성은 컴퓨터 과학의 초기에 임의의 값이 필요한 시뮬레이션과 게임 등에서 시작되었습니다.
2. 대안: JavaScript의 Math.random() 함수 이외에도 몇 가지 다른 방법으로 난수를 생성할 수 있습니다. 예를 들어, Random.js 라이브러리는 더 다양한 기능을 제공합니다.
3. 구현 세부사항: JavaScript의 Math.random() 함수는 실제로 난수가 아니라 의사 난수를 생성합니다. 의사 난수는 고정된 알고리즘을 사용하여 적절한 조작을 통해 생성된 값으로, 완전히 무작위로 보이지 않을 수 있습니다.

# 관련 정보:
- Math.random() 함수에 대한 자세한 내용은 공식 JavaScript 문서를 참조하면 됩니다.
- Random.js 라이브러리의 자세한 내용과 사용법은 해당 라이브러리의 공식 문서를 확인해 주세요.