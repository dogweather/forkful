---
title:                "Python: 테스트 작성"
programming_language: "Python"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/python/writing-tests.md"
---

{{< edit_this_page >}}

## 왜?

코드를 작성하는 것만으로는 충분하지 않습니다. 테스트를 작성하는 이유를 알아야 합니다. 테스트는 코드의 신뢰성을 확보하고 잠재적인 버그를 찾는 데 도움이 됩니다. 이러한 이유들로 인해, 테스트를 작성하는 것은 매우 중요합니다.

## 작성하는 법

테스트를 작성하는 방법은 매우 쉽습니다! 우선, 테스트를 하고자 하는 함수나 클래스를 만들어야 합니다. 그리고 나서, 입력값을 주고 예상한 결과를 가져오는 함수를 작성합니다. 예를 들어, 다음과 같습니다:

```Python
def multiply(a, b):
    return a * b

def test_multiply():
    assert multiply(2, 3) == 6
```

이렇게 하면, `test_multiply()` 함수에서 `assert` 구문으로 입력값을 2와 3으로 주고 실행한 결과가 6이 나오는지 검사합니다. 만약에 6이 나오지 않는다면, 테스트가 실패한 것입니다.

이제, 커맨드 라인에서 `pytest` 명령어를 입력하면 테스트 결과가 나옵니다:

```Bash
=============================== test session starts ===============================
platform darwin -- Python 3.8.4, pytest-5.4.3, py-1.9.0, pluggy-0.13.1
rootdir: /Users/john/doctest
collected 1 item 

doctest.py .                                                             [100%]

=============================== 1 passed in 0.34s ================================
```

이렇게 테스트를 작성하고 실행하면, 코드의 신뢰성과 안전성을 확보할 수 있습니다.

## 깊게 알아보기

물론, 테스트를 작성하는 과정에서 더 깊이 알아보아야 할 것들도 있습니다. 대부분의 테스트가 성공했을 때의 결과만을 확인하지만, 실패했을 때의 코드를 살펴볼 필요도 있습니다. 이를 통해 더 많은 버그를 찾을 수 있고, 코드의 품질을 향상시킬 수 있습니다. 또한, 테스트를 작성할 때 다양한 입력값과 예외 상황들을 고려하여 테스트 케이스를 구성하는 것도 중요합니다. 이를 통해 더 강력한 코드를 작성할 수 있습니다.

## 관련 정보

자세한 내용을 알고 싶다면 아래의 링크들을 참고해보세요:

- [pytest documentation](https://docs.pytest.org/en/stable/)
- [realpython: pytest](https://realpython.com/tutorials/testing/pytest/)
- [soohwan: 파이썬 테스트를 잘 작성하기 위한 알고리즘](https://soohwan.kr/python-test/)