---
title:                "테스트 작성하기"
html_title:           "Arduino: 테스트 작성하기"
simple_title:         "테스트 작성하기"
programming_language: "Python"
category:             "Python"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/python/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why? (무엇이며 왜?)
테스트 작성은 코드가 의도한 대로 실행되는지 확인하기 위한 과정입니다. 버그를 줄이고, 신뢰할 수 있는 소프트웨어를 만들기 위해 프로그래머들이 합니다.

## How to: (어떻게 하나요?)
```Python
import unittest

class TestSample(unittest.TestCase):
    def test_addition(self):
        result = 1 + 1
        self.assertEqual(result, 2)

if __name__ == '__main__':
    unittest.main()
```
출력:
```
.
----------------------------------------------------------------------
Ran 1 test in 0.000s

OK
```

## Deep Dive (심층 분석)
단위 테스트는 소프트웨어 개발의 초기부터 있었습니다. 대안으로는 pytest와 nose같은 도구들이 있죠. Python의 unittest 모듈은 자바의 JUnit에서 영향을 받았고, 테스트 케이스를 구조화하고 실행하는 표준화된 방법을 제공합니다.

## See Also (참고할 링크)
- Python 공식 문서 unittest 모듈: https://docs.python.org/3/library/unittest.html
- pytest 공식 웹사이트: https://docs.pytest.org/en/latest/
- nose 프로젝트 페이지: https://nose.readthedocs.io/en/latest/