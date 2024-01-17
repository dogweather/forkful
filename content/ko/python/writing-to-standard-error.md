---
title:                "표준 에러에 쓰는 방법"
html_title:           "Python: 표준 에러에 쓰는 방법"
simple_title:         "표준 에러에 쓰는 방법"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/python/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?
표준 에러 채널로 쓰기는 프로그래머들이 디버그 시에 필요한 정보를 화면에 출력하는 대신, 에러 메시지를 콘솔에 출력하는 것을 말합니다. 프로그래머들은 이를 통해 런타임 에러를 빠르게 찾고, 수정하기 쉽게 할 수 있습니다.

## 어떻게 하는가:
```Python
import sys

# 에러 메시지를 표준 에러 채널에 쓰는 예제
try:
    num = int(input("숫자를 입력하세요: "))
    print(num, "는 짝수입니다.")
except ValueError:
    sys.stderr.write("숫자를 입력하세요!")
```

```Python
# 터미널에서 해당 스크립트를 실행하면 STDERR 채널로 에러 메시지가 출력됩니다.
$ python STDERR_example.py
숫자를 입력하세요!
```

## 깊게 파고들기:
1. 표준 에러 채널에 대한 아이디어는 1975년에 UNIX 운영체제를 개발한 아이폰 레비와 켄 톰슨에 의해 처음 소개되었습니다.
2. 다른 대안으로는 로그 파일에 에러 메시지를 기록하는 것이 있습니다.
3. 표준 에러 채널에 쓰는 방법은 다음과 같이 두 가지 방식으로 구현할 수 있습니다:
   - ```sys.stderr.write()``` 함수를 사용하여 문자열을 직접 쓰는 방식
   - ```sys.excepthook()``` 함수를 재정의하여 예외가 발생했을 때 쓰는 방식

## 관련 자료:
- [Python 공식 문서](https://docs.python.org/3/library/sys.html#sys.stderr)
- [Real Python - Writing to Standard Error Channels](https://realpython.com/python-logging/#writing-to-standard-error-channels)