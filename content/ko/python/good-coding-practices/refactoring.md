---
aliases:
- /ko/python/refactoring/
date: 2024-01-26 03:37:28.543847-07:00
description: "\uB9AC\uD329\uD1A0\uB9C1\uC740 \uAE30\uC874\uC758 \uCEF4\uD4E8\uD130\
  \ \uCF54\uB4DC\uB97C \uC7AC\uAD6C\uC131\uD558\uB294 \uACFC\uC815\u2014\uC989 \uD329\
  \uD1A0\uB9C1\uC744 \uBCC0\uACBD\uD558\uB294 \uAC83\u2014\uC774\uBA70, \uC678\uBD80\
  \uC801\uC778 \uB3D9\uC791\uC740 \uBCC0\uACBD\uD558\uC9C0 \uC54A\uC2B5\uB2C8\uB2E4\
  . \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uCF54\uB4DC\uB97C \uC815\uB9AC\uD558\
  \uACE0, \uAC00\uB3C5\uC131\uC744 \uD5A5\uC0C1\uC2DC\uD0A4\uBA70, \uC0C8\uB85C\uC6B4\
  \ \uAE30\uB2A5\uC744 \uCD94\uAC00\uD558\uC9C0 \uC54A\uACE0\uB3C4 \uC720\uC9C0\uBCF4\
  \uC218\uC640 \uD655\uC7A5\uC744 \uC27D\uAC8C \uD558\uAE30 \uC704\uD574 \uC774 \uC791\
  \uC5C5\uC744 \uD569\uB2C8\uB2E4."
lastmod: 2024-02-18 23:09:05.638065
model: gpt-4-0125-preview
summary: "\uB9AC\uD329\uD1A0\uB9C1\uC740 \uAE30\uC874\uC758 \uCEF4\uD4E8\uD130 \uCF54\
  \uB4DC\uB97C \uC7AC\uAD6C\uC131\uD558\uB294 \uACFC\uC815\u2014\uC989 \uD329\uD1A0\
  \uB9C1\uC744 \uBCC0\uACBD\uD558\uB294 \uAC83\u2014\uC774\uBA70, \uC678\uBD80\uC801\
  \uC778 \uB3D9\uC791\uC740 \uBCC0\uACBD\uD558\uC9C0 \uC54A\uC2B5\uB2C8\uB2E4. \uD504\
  \uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uCF54\uB4DC\uB97C \uC815\uB9AC\uD558\uACE0\
  , \uAC00\uB3C5\uC131\uC744 \uD5A5\uC0C1\uC2DC\uD0A4\uBA70, \uC0C8\uB85C\uC6B4 \uAE30\
  \uB2A5\uC744 \uCD94\uAC00\uD558\uC9C0 \uC54A\uACE0\uB3C4 \uC720\uC9C0\uBCF4\uC218\
  \uC640 \uD655\uC7A5\uC744 \uC27D\uAC8C \uD558\uAE30 \uC704\uD574 \uC774 \uC791\uC5C5\
  \uC744 \uD569\uB2C8\uB2E4."
title: "\uB9AC\uD329\uD1A0\uB9C1"
---

{{< edit_this_page >}}

## 무엇 & 왜?
리팩토링은 기존의 컴퓨터 코드를 재구성하는 과정—즉 팩토링을 변경하는 것—이며, 외부적인 동작은 변경하지 않습니다. 프로그래머들은 코드를 정리하고, 가독성을 향상시키며, 새로운 기능을 추가하지 않고도 유지보수와 확장을 쉽게 하기 위해 이 작업을 합니다.

## 방법:
길이와 너비가 주어졌을 때 직사각형의 면적과 둘레를 계산하고 출력하는 코드 덩어리가 있다고 가정해보세요. 이 코드는 작업을 수행하지만, 반복적이고 조금 지저분합니다.

```python
# 원본 버전
length = 4
width = 3

# 면적과 둘레 계산
area = length * width
perimeter = 2 * (length + width)

print("면적:", area)
print("둘레:", perimeter)
```

이를 함수로 기능을 캡슐화하여 코드를 더 조직적이고 재사용 가능하게 리팩토링할 수 있습니다:

```python
# 리팩토링된 버전

def calculate_area(length, width):
    return length * width

def calculate_perimeter(length, width):
    return 2 * (length + width)

# 사용법
length = 4
width = 3

print("면적:", calculate_area(length, width))
print("둘레:", calculate_perimeter(length, width))
```

두 코드 조각 모두 같은 결과를 출력합니다:
```
면적: 12
둘레: 14
```

하지만 리팩토링된 버전은 더 깔끔하고 관심사를 분리하여, 다른 계산에 영향을 주지 않고 하나의 계산을 업데이트하기 쉽습니다.

## 깊이 있게
리팩토링은 소프트웨어 공학 초기 시대에 그 뿌리를 두고 있으며, 코드는 이미 "작동"하더라도 개선될 수 있고 개선되어야 한다는 것을 프로그래머들이 깨달았을 때 시작되었습니다. 마틴 파울러의 기념비적인 책 "리팩토링: 기존 코드의 설계 개선"은 많은 핵심 원칙과 기술을 명확히 했습니다. 그는 유명하게도 "어떤 바보라도 컴퓨터가 이해할 수 있는 코드를 작성할 수 있다. 좋은 프로그래머는 사람이 이해할 수 있는 코드를 작성한다."라고 말했습니다.

리팩토링의 대안으로는 처음부터 코드를 다시 작성하거나 체계적인 개선 없이 사소한 조정을 하는 것이 포함될 수 있습니다. 그러나, 리팩토링은 일반적으로 다시 작성하는 것보다 비용 효율적이며, 즉흥적인 수정보다 위험이 적습니다. 구현 세부 사항은 프로그래밍 패러다임마다 특정할 수 있지만, 객체 지향 프로그래밍은 특히 리팩토링, 특히 메서드 추출(`calculate_area` 및 `calculate_perimeter` 함수와 같은), 인라인화, 객체 간 기능 이동, 이름 변경 메서드나 변수의 명확성을 높이는 기술에 특히 적합합니다.

Python에서의 리팩토링은  `PyCharm`과 같이 내장된 리팩토링 기능을 가진 도구나 리팩토링을 위해 특별히 설계된 Python 라이브러리인 `rope`을 자주 사용합니다. 리팩토링하는 동안 `git`과 같은 버전 관리의 신중한 사용이 변화를 점진적으로 추적하는 데 강력히 권장됩니다.

## 또한 보기
더 알고 싶은 분들을 위한 추천 자료:
- 마틴 파울러의 책: [리팩토링: 기존 코드의 설계 개선](http://www.refactoring.com/)
- Python 리팩토링 with `rope`: [GitHub - rope](https://github.com/python-rope/rope)
- PyCharm 리팩토링 문서: [Jetbrains PyCharm 리팩토링 소스 코드](https://www.jetbrains.com/help/pycharm/refactoring-source-code.html)
- Refactoring.guru: [리팩토링과 디자인 패턴](https://refactoring.guru/refactoring)
- 삼촌 밥 (로버트 C. 마틴)의 클린 코드 강의: [클린 코드 - 삼촌 밥 / 레슨 1](https://www.youtube.com/watch?v=7EmboKQH8lM)
