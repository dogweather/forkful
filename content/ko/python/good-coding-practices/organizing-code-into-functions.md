---
date: 2024-01-26 01:16:32.770270-07:00
description: "\uBC29\uBC95: \uC22B\uC790\uC758 \uC81C\uACF1\uACFC \uC138\uC81C\uACF1\
  \uC744 \uACC4\uC0B0\uD558\uB294 \uC2A4\uD06C\uB9BD\uD2B8\uB97C \uC791\uC131\uD55C\
  \uB2E4\uACE0 \uAC00\uC815\uD574 \uBD05\uC2DC\uB2E4. \uD568\uC218\uAC00 \uC5C6\uC73C\
  \uBA74 \uC774\uAC83\uC740 \uBC18\uBCF5\uC758 \uBB34\uB354\uAE30\uC785\uB2C8\uB2E4\
  ."
lastmod: '2024-03-13T22:44:54.605898-06:00'
model: gpt-4-0125-preview
summary: "\uC22B\uC790\uC758 \uC81C\uACF1\uACFC \uC138\uC81C\uACF1\uC744 \uACC4\uC0B0\
  \uD558\uB294 \uC2A4\uD06C\uB9BD\uD2B8\uB97C \uC791\uC131\uD55C\uB2E4\uACE0 \uAC00\
  \uC815\uD574 \uBD05\uC2DC\uB2E4."
title: "\uCF54\uB4DC\uB97C \uD568\uC218\uB85C \uAD6C\uC131\uD558\uAE30"
weight: 18
---

## 방법:
숫자의 제곱과 세제곱을 계산하는 스크립트를 작성한다고 가정해 봅시다. 함수가 없으면 이것은 반복의 무더기입니다:

```Python
num = 4
square = num * num
cube = num * num * num
print(f"제곱: {square}, 세제곱: {cube}")

num = 5
square = num * num
cube = num * num * num
print(f"제곱: {square}, 세제곱: {cube}")
```
출력:
```
제곱: 16, 세제곱: 64
제곱: 25, 세제곱: 125
```

함수를 사용하면 더 깔끔합니다:

```Python
def square(n):
    return n * n

def cube(n):
    return n ** 3

num = 4
print(f"제곱: {square(num)}, 세제곱: {cube(num)}")

num = 5
print(f"제곱: {square(num)}, 세제곱: {cube(num)}")
```
출력:
```
제곱: 16, 세제곱: 64
제곱: 25, 세제곱: 125
```

## 깊이 들어가기
예전에 프로그램이 간단했을 때는, 명령어 목록을 작성하는 것만으로도 충분했습니다. 하지만 소프트웨어가 더 복잡해지면서 개발자들은 같은 코드를 계속해서 다시 작성하고 있다는 것을 깨달았습니다. 이에 함수가 등장했습니다—단일 작업을 수행하는 재사용 가능한 코드 블록입니다.

함수의 대안으로는 클래스(작동하는 데이터와 함께 함수를 묶는 것)와 인라인 코드(필요한 곳에 바로 지능을 적용하지만 복잡한 작업에는 위험한 방법)가 있습니다. 구현 측면에서의 요령은 단지 함수를 만드는 것이 아니라 이들을 하나의 일을 잘 하도록 만드는 것입니다—단일 책임 원칙을 생각해 보세요. 함수는 또한 이상적으로는 상태 없이(stateless) 있어야 하며, 들어오거나 나가는 데이터와 관련된 놀라움이 없어야 합니다.

## 또한 보기
- 함수에 대한 공식 Python 튜토리얼: https://docs.python.org/3/tutorial/controlflow.html#defining-functions
- 깨끗한 함수를 작성하는 원칙에 관한 'Clean Code' by Robert C. Martin.
- 코드 구성의 예제를 포함한 'Refactoring: Improving the Design of Existing Code' by Martin Fowler.
