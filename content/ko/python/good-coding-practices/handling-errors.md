---
date: 2024-01-26 00:57:19.381929-07:00
description: "\uD30C\uC774\uC36C\uC5D0\uC11C \uC5D0\uB7EC\uB97C \uCC98\uB9AC\uD558\
  \uB294 \uAC83(\uD639\uC740 \uADF8 \uC5B4\uB5A4 \uD504\uB85C\uADF8\uB798\uBC0D \uC5B8\
  \uC5B4\uC5D0\uC11C\uB4E0)\uC740 \uB73B\uBC16\uC758 \uC77C\uC744 \uC608\uC0C1\uD558\
  \uB294 \uAC83\uC5D0 \uAD00\uD55C \uAC83\uC785\uB2C8\uB2E4 - \uCF54\uB4DC \uC0C1\uD0DC\
  \uAC00 \uC88B\uC9C0 \uC54A\uC744 \uB54C \uC6B0\uC544\uD558\uAC8C \uAD00\uB9AC\uD558\
  \uB294 \uC2A4\uD0AC\uC758 \uC77C\uBD80\uC785\uB2C8\uB2E4. \uC6B0\uB9AC\uB294 \uD504\
  \uB85C\uADF8\uB7A8\uC774 \uCDA9\uB3CC\uD558\uB294 \uC77C\uC744 \uBC29\uC9C0\uD558\
  \uACE0, \uC0AC\uC6A9\uC790\uB97C \uC548\uB0B4\uD558\uBA70, \uD504\uB85C\uADF8\uB7A8\
  \uC744 \uACAC\uACE0\uD558\uACE0 \uC2E0\uB8B0\uD560 \uC218\u2026"
lastmod: '2024-02-25T18:49:51.645704-07:00'
model: gpt-4-1106-preview
summary: "\uD30C\uC774\uC36C\uC5D0\uC11C \uC5D0\uB7EC\uB97C \uCC98\uB9AC\uD558\uB294\
  \ \uAC83(\uD639\uC740 \uADF8 \uC5B4\uB5A4 \uD504\uB85C\uADF8\uB798\uBC0D \uC5B8\uC5B4\
  \uC5D0\uC11C\uB4E0)\uC740 \uB73B\uBC16\uC758 \uC77C\uC744 \uC608\uC0C1\uD558\uB294\
  \ \uAC83\uC5D0 \uAD00\uD55C \uAC83\uC785\uB2C8\uB2E4 - \uCF54\uB4DC \uC0C1\uD0DC\
  \uAC00 \uC88B\uC9C0 \uC54A\uC744 \uB54C \uC6B0\uC544\uD558\uAC8C \uAD00\uB9AC\uD558\
  \uB294 \uC2A4\uD0AC\uC758 \uC77C\uBD80\uC785\uB2C8\uB2E4. \uC6B0\uB9AC\uB294 \uD504\
  \uB85C\uADF8\uB7A8\uC774 \uCDA9\uB3CC\uD558\uB294 \uC77C\uC744 \uBC29\uC9C0\uD558\
  \uACE0, \uC0AC\uC6A9\uC790\uB97C \uC548\uB0B4\uD558\uBA70, \uD504\uB85C\uADF8\uB7A8\
  \uC744 \uACAC\uACE0\uD558\uACE0 \uC2E0\uB8B0\uD560 \uC218\u2026"
title: "\uC5D0\uB7EC \uCC98\uB9AC\uD558\uAE30"
---

{{< edit_this_page >}}

## 무엇과 왜?

파이썬에서 에러를 처리하는 것(혹은 그 어떤 프로그래밍 언어에서든)은 뜻밖의 일을 예상하는 것에 관한 것입니다 - 코드 상태가 좋지 않을 때 우아하게 관리하는 스킬의 일부입니다. 우리는 프로그램이 충돌하는 일을 방지하고, 사용자를 안내하며, 프로그램을 견고하고 신뢰할 수 있도록 만들기 위해 이를 수행합니다.

## 어떻게 할까:

``` Python
# 기본 try-except 블록
try:
    # 위험한 코드
    number = int(input("숫자를 입력하세요: "))
except ValueError:
    # 에러를 처리
    print("그건 숫자가 아닙니다!")

# 여러 예외 명시하기
try:
    # 다양한 예외를 일으킬 수 있는 코드
    result = 10 / int(input("나눗수를 입력하세요: "))
except ZeroDivisionError:
    print("이런! 0으로 나눌 수 없습니다.")
except ValueError:
    print("친구, 숫자가 필요해요.")

# else와 finally 사용하기
try:
    number = int(input("제곱할 숫자를 입력하세요: "))
except ValueError:
    print("숫자라고 했습니다!")
else:
    # 에러가 발생하지 않음
    print("당신의 숫자의 제곱은:", number**2)
finally:
    # 항상 실행
    print("이것을 시도해줘서 감사합니다!")
```

첫 번째 블록에 유효하지 않은 숫자를 입력했을 때의 예시 출력:
```
숫자를 입력하세요: 안녕
그건 숫자가 아닙니다!
```

## 깊이 파기

프로그래밍의 시작부터 에러 처리는 필수적이었습니다. 초기 접근법은 모든 위험한 작업 전에 조건을 확인하는 것과 같이 기본적이었습니다. 파이썬의 `try-except` 구문은 C++ 및 Java와 같은 오래된 언어의 예외 처리 유산에서 비롯되었으며, 이 과정을 단순화했습니다.

코드 블록을 `try`할 때, 파이썬은 모든 예외를 감시합니다. 에러가 발생하면 `except` 블록이 그것을 잡습니다. 당신은 잡고자 하는 구체적인 예외를 정할 수 있으며, 맨 `except`를 쓰면 모든 예외를 포착할 수 있습니다. 그러나, 구체적인 것부터 그것이 더 나은 접근법 입니다 - 그것은 정확하고, 모든 것을 잡는 그물이 아닙니다.

`else`와 `finally`는 이 개념에서 추가된 것들입니다. `else` 블록은 try 블록이 에러 없이 실행됐을 때 실행됩니다. `finally`는 무슨 일이 있어도 실행되는 믿을 수 있는 친구입니다 - 예를 들면, 정리 작업을 생각해보세요.

대안이 있습니까? 그럼요, 있죠. 일부 언어는 예외 대신 반환 코드를 사용합니다. 리소스를 처리하기 위한 `with` 구문이나 개발하는 동안 조건을 확인하는 `assertions`을 마주칠 수도 있습니다. 그러나 우리가 견고한 에러 처리 전략에 대해 이야기할 때, try-catch 모델은 그 가독성과 구조로 인해 두드러집니다.

## 참조하기

더 깊이 파고들기 위한 좋은 추가 자료들은 다음과 같습니다:

- 파이썬 공식 문서의 에러와 예외에 대해서: [파이썬 문서 – 에러와 예외](https://docs.python.org/3/tutorial/errors.html)
- 이 주제에 대한 Real Python의 안내서: [Real Python - try/except/else/finally 블록](https://realpython.com/python-exceptions/)
- 에러 처리 모범 사례에 대한 심도 있는 토론: [Stack Overflow – 제대로 예외를 무시하는 방법은?](https://stackoverflow.com/questions/4990718/about-catching-any-exception)
