---
title:                "테스트 작성하기"
html_title:           "Python: 테스트 작성하기"
simple_title:         "테스트 작성하기"
programming_language: "Python"
category:             "Python"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/python/writing-tests.md"
---

{{< edit_this_page >}}

## 왜

테스트를 작성하는 이유는 개발한 코드가 정확하고 예상된 대로 작동하는지 검증하기 위해서입니다. 테스트를 꼼꼼하게 작성하면 코드의 오류를 발견하고 수정할 수 있어서 더욱 신뢰성 높은 프로그램을 만들 수 있습니다.

## 어떻게 작성할까요?

```python
# 예시: 단위 테스트 작성 방법

# 간단한 함수
def add(a, b):
    return a + b

# add 함수에 대한 단위 테스트
assert add(2, 3) == 5
assert add(5, 10) == 15
assert add(-1, 6) == 5
```

```python
# 예시: 통합 테스트 작성 방법

# 간단한 기능을 가진 웹 어플리케이션
from flask import Flask 

app = Flask(__name__)

@app.route('/')
def hello():
    return 'Hello World!'

# '/' 경로에 대한 통합 테스트
from flask_testing import LiveServerTestCase 
from selenium import webdriver

class TestWebApp(LiveServerTestCase):
    # 웹 브라우저 설정
    def create_app(self):
        app.config['TESTING'] = True
        app.config['LIVESERVER_PORT'] = 0 # 임의의 포트 지정
        return app

    # 테스트 전에 웹 브라우저 실행
    def setUp(self):
        self.driver = webdriver.Chrome()
        self.driver.get(self.get_server_url())

    # 테스트 후 웹 브라우저 종료
    def tearDown(self):
        self.driver.quit()

    # 페이지 내용 테스트
    def test_hello(self):
        self.driver.find_element_by_tag_name('button').click()
        assert 'Hello World!' in self.driver.page_source
```

## 딥 다이브 

테스트의 종류에는 단위 테스트, 통합 테스트, 기능 테스트 등이 있으며, 각각의 장단점이 있습니다. 또한 테스트를 자동화하는 방법으로는 unittest 라이브러리, pytest 라이브러리 등이 있습니다. 정확한 테스트 작성을 위해선 테스트 코드를 작성하기 전에 예상되는 출력 값을 먼저 정의하는 것이 중요합니다.

## 참고

- [테스트 주도 개발: 우리는 왜 테스트를 할까](https://medium.com/@tyson_swartz/tdd-why-we-test-d04bbc089ebf)
- [파이썬 단위 테스트: 왜하고 어떻게 하나요?](https://www.slideshare.net/ByoungYoulYu/sds-ug-38026141)
- [파이썬으로 쉽게 배우는 TDD](https://wikidocs.net/15650)