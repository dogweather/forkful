---
changelog:
- 2024-02-22, dogweather, reviewed
- 2024-02-22, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-02-22 17:30:02.756263-07:00
description: "\uC0C8\uB85C\uC6B4 Python \uD504\uB85C\uC81D\uD2B8\uB97C \uC2DC\uC791\
  \uD558\uB294 \uAC83\uC740 \uCC98\uC74C\uBD80\uD130 \uAD6C\uC870\uD654\uB418\uACE0\
  \ \uC720\uC9C0\uBCF4\uC218 \uAC00\uB2A5\uD55C \uD504\uB808\uC784\uC6CC\uD06C\uB97C\
  \ \uC124\uC815\uD558\uB294 \uAC83\uC5D0 \uAD00\uD55C \uAC83\uC785\uB2C8\uB2E4. \uD504\
  \uB85C\uADF8\uB798\uBA38\uB294 \uC774\uAC83\uC744 \uD1B5\uD574 \uCF54\uB4DC\uAC00\
  \ \uC77D\uAE30 \uC27D\uACE0, \uB514\uBC84\uADF8\uD558\uAE30 \uC27D\uACE0, \uD2B9\
  \uD788 \uD504\uB85C\uC81D\uD2B8\uC640 \uADF8\uAC83\uC744 \uC791\uC5C5\uD558\uB294\
  \ \uD300\uC774 \uC2DC\uAC04\uC774 \uC9C0\uB0A8\uC5D0 \uB530\uB77C \uC131\uC7A5\uD568\
  \uC5D0 \uB530\uB77C \uD611\uC5C5\uD558\uAE30 \uC27D\uB3C4\uB85D\u2026"
lastmod: '2024-02-25T18:49:51.635433-07:00'
model: gpt-4-0125-preview
summary: "\uC0C8\uB85C\uC6B4 Python \uD504\uB85C\uC81D\uD2B8\uB97C \uC2DC\uC791\uD558\
  \uB294 \uAC83\uC740 \uCC98\uC74C\uBD80\uD130 \uAD6C\uC870\uD654\uB418\uACE0 \uC720\
  \uC9C0\uBCF4\uC218 \uAC00\uB2A5\uD55C \uD504\uB808\uC784\uC6CC\uD06C\uB97C \uC124\
  \uC815\uD558\uB294 \uAC83\uC5D0 \uAD00\uD55C \uAC83\uC785\uB2C8\uB2E4. \uD504\uB85C\
  \uADF8\uB798\uBA38\uB294 \uC774\uAC83\uC744 \uD1B5\uD574 \uCF54\uB4DC\uAC00 \uC77D\
  \uAE30 \uC27D\uACE0, \uB514\uBC84\uADF8\uD558\uAE30 \uC27D\uACE0, \uD2B9\uD788 \uD504\
  \uB85C\uC81D\uD2B8\uC640 \uADF8\uAC83\uC744 \uC791\uC5C5\uD558\uB294 \uD300\uC774\
  \ \uC2DC\uAC04\uC774 \uC9C0\uB0A8\uC5D0 \uB530\uB77C \uC131\uC7A5\uD568\uC5D0 \uB530\
  \uB77C \uD611\uC5C5\uD558\uAE30 \uC27D\uB3C4\uB85D\u2026"
title: "\uC0C8 \uD504\uB85C\uC81D\uD2B8 \uC2DC\uC791\uD558\uAE30"
---

{{< edit_this_page >}}

## 무엇을, 왜?

새로운 Python 프로젝트를 시작하는 것은 처음부터 구조화되고 유지보수 가능한 프레임워크를 설정하는 것에 관한 것입니다. 프로그래머는 이것을 통해 코드가 읽기 쉽고, 디버그하기 쉽고, 특히 프로젝트와 그것을 작업하는 팀이 시간이 지남에 따라 성장함에 따라 협업하기 쉽도록 합니다.

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
