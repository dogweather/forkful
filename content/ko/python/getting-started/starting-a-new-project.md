---
changelog:
- 2024-02-22, dogweather, reviewed
- 2024-02-22, gpt-4-0125-preview, translated from English
date: 2024-02-22 17:30:02.756263-07:00
description: "\uC5B4\uB5BB\uAC8C: \uAC00\uC0C1 \uD658\uACBD\uC740 Python \uD504\uB85C\
  \uC81D\uD2B8\uAC00 \uD544\uC694\uB85C \uD558\uB294 \uD328\uD0A4\uC9C0\uB97C \uC0AC\
  \uC6A9\uD558\uAE30 \uC704\uD55C \uD544\uC218 \uC2E4\uD589 \uD30C\uC77C\uC744 \uD3EC\
  \uD568\uD55C \uB3C5\uB9BD\uC801\uC778 \uB514\uB809\uD1A0\uB9AC\uC785\uB2C8\uB2E4\
  . \uD504\uB85C\uC81D\uD2B8 \uAC04\uC758 \uC885\uC18D\uC131 \uCDA9\uB3CC\uC744 \uD53C\
  \uD558\uAE30 \uC704\uD574 \uAC01 \uD504\uB85C\uC81D\uD2B8\uC5D0 \uB300\uD574 \uAC00\
  \uC0C1 \uD658\uACBD\uC744 \uC0DD\uC131\uD558\uB294 \uAC83\uC774 \uC88B\uC2B5\uB2C8\
  \uB2E4. \uD45C\uC900 Python \uB77C\uC774\uBE0C\uB7EC\uB9AC\uC758 \uC77C\uBD80\uC778\
  \u2026"
lastmod: '2024-03-13T22:44:54.598849-06:00'
model: gpt-4-0125-preview
summary: "\uAC00\uC0C1 \uD658\uACBD\uC740 Python \uD504\uB85C\uC81D\uD2B8\uAC00 \uD544\
  \uC694\uB85C \uD558\uB294 \uD328\uD0A4\uC9C0\uB97C \uC0AC\uC6A9\uD558\uAE30 \uC704\
  \uD55C \uD544\uC218 \uC2E4\uD589 \uD30C\uC77C\uC744 \uD3EC\uD568\uD55C \uB3C5\uB9BD\
  \uC801\uC778 \uB514\uB809\uD1A0\uB9AC\uC785\uB2C8\uB2E4."
title: "\uC0C8 \uD504\uB85C\uC81D\uD2B8 \uC2DC\uC791\uD558\uAE30"
weight: 1
---

## 어떻게:


### 가상 환경 만들기
가상 환경은 Python 프로젝트가 필요로 하는 패키지를 사용하기 위한 필수 실행 파일을 포함한 독립적인 디렉토리입니다. 프로젝트 간의 종속성 충돌을 피하기 위해 각 프로젝트에 대해 가상 환경을 생성하는 것이 좋습니다. 표준 Python 라이브러리의 일부인 `venv` 모듈을 사용합니다.

```shell
# 'myproject'를 프로젝트의 이름으로 교체하세요
python3 -m venv myproject-env
```

가상 환경을 활성화하려면:

Windows에서:
```shell
myproject-env\Scripts\activate.bat
```

Unix 또는 MacOS에서:
```shell
source myproject-env/bin/activate
```

샘플 출력 (운영 체제에 따라 출력이 약간 다를 수 있습니다):
```shell
(myproject-env) $
```

### 패키지 설치하기
Python용 패키지 설치 프로그램인 `pip`을 사용하여 패키지를 설치, 업그레이드 및 제거합니다. 여기 HTTP 요청을 만들기 위해 인기 있는 서드파티 라이브러리인 `requests`를 설치하는 방법입니다:

```shell
pip install requests
```

샘플 출력:
```shell
Collecting requests
  Downloading requests-2.25.1-py2.py3-none-any.whl (61 kB)
     |████████████████████████████████| 61 kB 1.3 MB/s
Installing collected packages: requests
Successfully installed requests-2.25.1
```

### 프로젝트 구조 설정하기
전형적인 Python 프로젝트는 다음과 같을 수 있습니다:

```
myproject/
│
├── myproject-env/    # 가상 환경
├── docs/             # 문서
├── tests/            # 단위 및 통합 테스트
│   └── __init__.py
├── myproject/        # 프로젝트 소스 코드
│   ├── __init__.py
│   └── main.py
├── setup.py          # 프로젝트 설정 파일
└── README.md         # 프로젝트 개요
```

### 첫 번째 프로그램 만들기
`myproject` 디렉토리 내부에 `main.py` 파일을 생성합니다. 여기 간단한 프로그램의 예가 있습니다:

```python
# myproject/myproject/main.py
def greet(name):
    return f"Hello, {name}!"

if __name__ == "__main__":
    print(greet("World"))
```

프로그램 실행하기:

```shell
python myproject/main.py
```

샘플 출력:
```shell
Hello, World!
```

### 더 큰 프로젝트를 위해 프레임워크 사용하기
특히 웹 어플리케이션과 같은 더 큰 프로젝트의 경우, Django나 Flask와 같은 프레임워크가 매우 유용합니다. Flask를 설치하고 간단한 "Hello, World" 웹 어플리케이션을 생성하는 방법은 다음과 같습니다:

```shell
pip install Flask
```

다음 내용을 포함하는 `app.py` 파일을 생성합니다:

```python
# app.py
from flask import Flask
app = Flask(__name__)

@app.route("/")
def hello_world():
    return "<p>Hello, World!</p>"

if __name__ == "__main__":
    app.run(debug=True)
```

Flask 어플리케이션 실행하기:

```shell
flask run
```

샘플 출력:
```shell
 * Running on http://127.0.0.1:5000/ (Press CTRL+C to quit)
```

웹 브라우저에서 `http://127.0.0.1:5000/`로 이동하면 "Hello, World!" 메시지를 볼 수 있습니다.
