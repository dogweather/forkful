---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:31:43.313876-07:00
description: "\uBC29\uBC95: Python\uC740 `unittest`\uB77C\uB294 \uB0B4\uC7A5 \uBAA8\
  \uB4C8\uC744 \uD1B5\uD574 \uD14C\uC2A4\uD2B8\uB97C \uC791\uC131\uD560 \uC218 \uC788\
  \uC2B5\uB2C8\uB2E4. \uC774\uAC83\uC774 \uAC04\uB2E8\uD55C \uD568\uC218\uB97C \uD14C\
  \uC2A4\uD2B8\uD558\uB294 \uBC29\uBC95\uC785\uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:54.603061-06:00'
model: gpt-4-0125-preview
summary: "Python\uC740 `unittest`\uB77C\uB294 \uB0B4\uC7A5 \uBAA8\uB4C8\uC744 \uD1B5\
  \uD574 \uD14C\uC2A4\uD2B8\uB97C \uC791\uC131\uD560 \uC218 \uC788\uC2B5\uB2C8\uB2E4\
  ."
title: "\uD14C\uC2A4\uD2B8 \uC791\uC131\uD558\uAE30"
weight: 36
---

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
