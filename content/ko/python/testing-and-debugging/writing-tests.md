---
title:                "테스트 작성하기"
aliases:
- /ko/python/writing-tests/
date:                  2024-02-03T19:31:43.313876-07:00
model:                 gpt-4-0125-preview
simple_title:         "테스트 작성하기"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/python/writing-tests.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 무엇과 왜?
Python에서 테스트를 작성하는 것은 코드의 정확성을 검증하기 위해 자동 스크립트를 만드는 과정을 말합니다. 프로그래머들은 이러한 작업을 통해 다양한 조건에서 함수나 클래스가 예상대로 작동하는지 확인함으로써, 오류를 조기에 발견하고 유지보수 및 리팩터링을 용이하게 합니다.

## 방법:
Python은 `unittest`라는 내장 모듈을 통해 테스트를 작성할 수 있습니다. 이것이 간단한 함수를 테스트하는 방법입니다:

```python
import unittest

def add(a, b):
    return a + b

class TestAddFunction(unittest.TestCase):
    def test_add(self):
        self.assertEqual(add(1, 2), 3)
        self.assertEqual(add(-1, 1), 0)
        self.assertNotEqual(add(10, 2), 12, "12여야 합니다.")

if __name__ == '__main__':
    unittest.main()
```

이 테스트 스크립트를 실행하면, 테스트가 통과했는지(또는 실패했는지)를 나타내는 출력을 볼 수 있습니다.

더 현대적이고 표현력 있는 테스트를 위해서는 `pytest`와 같은 타사 라이브러리를 사용할 수 있습니다. 먼저 pip를 사용하여 설치해야 합니다:

```shell
pip install pytest
```

그런 다음, 아무것도 서브클래스로 만들 필요 없이 테스트를 더 간단하게 작성할 수 있습니다:

```python
# test_with_pytest.py라는 파일에 이것을 저장하세요
def add(a, b):
    return a + b

def test_add():
    assert add(1, 2) == 3
    assert add(-1, 1) == 0
    assert add(10, 2) != 12, "12여야 합니다."
```

`pytest`로 테스트를 실행하려면, 단순히 다음을 실행하세요:

```shell
pytest test_with_pytest.py
```

pytest에서 나오는 테스트 결과를 보여주는 출력을 볼 수 있습니다.
