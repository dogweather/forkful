---
date: 2024-01-26 01:16:22.080359-07:00
description: "\uBC29\uBC95: \uD568\uC218\uC758 \uAC1C\uB150, \uB8E8\uBE44\uC5D0\uC11C\
  \uB294 \uBA54\uC18C\uB4DC\uB85C \uC54C\uB824\uC838 \uC788\uC2B5\uB2C8\uB2E4, \uC0C8\
  \uB85C\uC6B4 \uAC83\uC774 \uC544\uB2D9\uB2C8\uB2E4 - \uD504\uB85C\uADF8\uB798\uBC0D\
  \ \uC790\uCCB4\uB9CC\uD07C \uC624\uB798\uB410\uC2B5\uB2C8\uB2E4. 1950\uB144\uB300\
  \uB85C \uB3CC\uC544\uAC00\uC11C, \uADF8 \uB2F9\uC2DC\uC5D0\uB294 \uC11C\uBE0C\uB8E8\
  \uD2F4\uC73C\uB85C \uC54C\uB824\uC838 \uC788\uC5C8\uB358 \uAC83\uB4E4\uC774 \uC911\
  \uBCF5\uC744 \uC904\uC774\uAE30 \uC704\uD574 \uB3C4\uC785\uB418\uC5C8\uC2B5\uB2C8\
  \uB2E4. \uB300\uC548\uC774 \uC788\uB2E4\uAD6C\uC694? \uBB3C\uB860\uC785\uB2C8\uB2E4\
  , \uC778\uB77C\uC778 \uCF54\uB4DC\uAC00 \uC788\uC744\u2026"
lastmod: '2024-04-05T22:51:10.174811-06:00'
model: gpt-4-0125-preview
summary: "\uD568\uC218\uC758 \uAC1C\uB150, \uB8E8\uBE44\uC5D0\uC11C\uB294 \uBA54\uC18C\
  \uB4DC\uB85C \uC54C\uB824\uC838 \uC788\uC2B5\uB2C8\uB2E4, \uC0C8\uB85C\uC6B4 \uAC83\
  \uC774 \uC544\uB2D9\uB2C8\uB2E4 - \uD504\uB85C\uADF8\uB798\uBC0D \uC790\uCCB4\uB9CC\
  \uD07C \uC624\uB798\uB410\uC2B5\uB2C8\uB2E4."
title: "\uCF54\uB4DC\uB97C \uD568\uC218\uB85C \uAD6C\uC131\uD558\uAE30"
weight: 18
---

## 방법:
사용자에게 인사하는 간단한 스크립트를 작성한다고 상상해 보세요:

```Ruby
def greet(name)
  "안녕하세요, #{name}!"
end

puts greet("Alice")   # 출력: 안녕하세요, Alice!
puts greet("Bob")     # 출력: 안녕하세요, Bob!
```

또는 원의 면적을 계산할 수도 있습니다:

```Ruby
def circle_area(radius)
  Math::PI * radius ** 2
end

puts circle_area(5)   # 출력: 78.53981633974483
```

더 깔끔하고 다루기 쉽죠?

## 깊이 탐구
함수의 개념, 루비에서는 메소드로 알려져 있습니다, 새로운 것이 아닙니다 - 프로그래밍 자체만큼 오래됐습니다. 1950년대로 돌아가서, 그 당시에는 서브루틴으로 알려져 있었던 것들이 중복을 줄이기 위해 도입되었습니다.

대안이 있다구요? 물론입니다, 인라인 코드가 있을 수 있고, 클래스와 객체를 사용하여 OOP로 갈 수도 있으며, 람다와 프록스를 사용하여 함수형으로 갈 수도 있습니다. 그러나 함수는 정돈된 코드의 기본입니다. 성능을 원하십니까? 함수 내의 지역 변수는 빠르고, 함수는 `return`을 사용하여 즉시 값을 반환할 수 있습니다.

구현 측면에서, `def`로 함수를 정의하고 `end`로 끝낼 수 있습니다. 기본 매개변수를 설정할 수 있고, 가변 함수에 스플랫 연산자를 사용할 수 있으며, 그 이상도 가능합니다. 함수는 원하는 만큼 단순하거나 복잡할 수 있습니다.

## 참고
- [루비의 메소드 문서화](https://ruby-doc.org/core-2.7.0/Method.html)
- [Chris Pine의 프로그래밍 배우기](https://pine.fm/LearnToProgram/)
- [Sandi Metz의 Ruby에서 실용적인 객체 지향 디자인](https://www.poodr.com/)
