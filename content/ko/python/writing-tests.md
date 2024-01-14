---
title:    "Python: 테스트 작성하기"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/python/writing-tests.md"
---

{{< edit_this_page >}}

# 왜: 왜 테스트 코드를 작성하는 것이 중요한지 간단히 설명합니다.

사실, 테스트 코드는 개발 과정에서 가장 중요한 부분입니다. 왜냐하면 테스트 코드를 작성함으로써 우리는 우리가 작성한 코드에 대한 믿음을 갖게 되고, 코드의 품질을 높일 수 있기 때문입니다. 또한, 테스트 코드는 버그를 예방하고 디버깅 시간을 줄여줍니다.

## 어떻게: 테스트 코드를 작성하는 방법

```Python
# 간단한 함수
def add(num1, num2):
    return num1 + num2

# 함수의 테스트 코드
def test_add():
    assert add(2, 3) == 5
    assert add(5, 7) == 12
    assert add(-1, 1) == 0
```

이렇게 간단한 함수의 경우에는 모든 경우의 수를 테스트할 수 있지만, 실제로는 더 복잡한 함수를 다루게 됩니다. 이때는 더 다양한 테스트 케이스를 고려해야 합니다.

```Python
# 복잡한 함수
def divide(num1, num2):
    # 두 수를 나누기 전 0인지 체크
    if num2 == 0:
        raise ValueError("Cannot divide by zero!")
    else:
        return num1/num2

# 함수의 테스트 코드
def test_divide():
    # 예외 처리 테스트
    try:
        divide(10, 0)
    except ValueError:
        pass
    # 일반적인 경우 테스트
    assert divide(10, 2) == 5
    assert divide(10, 5) == 2
    assert divide(5, 10) == 0.5
```

이렇게 다양한 테스트 케이스를 고려하여 코드를 작성하면 예상치 못한 버그를 방지할 수 있습니다.

## 딥 다이브: 테스트 코드에 대해 더 알아보기

테스트 코드를 작성할 때는 여러 가지 기능을 활용할 수 있습니다. 예를 들어, `assert` 문을 사용해서 조건문이 참인지 검증할 수 있고, 예외 처리를 통해 테스트를 하는 것도 가능합니다. 또한, `pytest`와 같은 파이썬 라이브러리를 사용하면 더욱 효율적이고 체계적인 테스트를 할 수 있습니다.

# 또한 보기

[Python으로 테스트 코드 작성하기](https://www.edwith.org/boostcourse-cs-test-driven-development/lecture/60105/)  
[Pytest: 파이썬으로 단위 테스트 작성하기](https://velog.io/@leejh3224/pytest-follow-up-post)