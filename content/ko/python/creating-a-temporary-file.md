---
title:                "임시 파일 생성하기"
html_title:           "Python: 임시 파일 생성하기"
simple_title:         "임시 파일 생성하기"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/python/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

임시 파일 생성은 컴퓨터에 일시적 파일을 만드는 과정입니다. 이렇게 하는 이유는 일시적으로 데이터를 저장하거나 처리가 필요할 때, 또는 마치 파일처럼 동작하는 메모리 저장 공간이 필요할 때 입니다.

## 어떻게:

Python에서는 `tempfile` 라이브러리를 사용해 쉽게 임시 파일을 생성할 수 있습니다. 예컨대 아래와 같이 사용할 수 있습니다:

```Python
import tempfile

temp = tempfile.TemporaryFile()
temp.write(b'Some data')
temp.seek(0)

print(temp.read())
temp.close()
```

이 코드를 실행하면 'Some data'라는 문자열이 저장된 임시 파일이 생성됩니다.

## Deep Dive:

임시 파일 생성은 오래된 개념이며, 이런 방식으로 데이터를 일시적으로 처리하는 것은 컴퓨터 세계에서 흔히 볼 수 있는 방식입니다. Python에서는 이를 매우 간단하게 처리할 수 있는 `tempfile` 라이브러리를 제공합니다. 

대안으로는, 문자열을 메모리에 저장하는 방식이나, 직접 전용의 일시적 저장소를 설계하는 방법 등이 있습니다. 하지만 대부분의 경우, 파이썬의 `tempfile` 라이브러리로 충분히 임시 파일 저장 공간을 관리할 수 있습니다.

`tempfile` 라이브러리의 임시 파일은 기본적으로 바이너리 모드(w+b)으로 열립니다. 원한다면 텍스트 모드를 사용할 수도 있지만, 이 경우 인코딩도 같이 설정해야 합니다. 

## 참고자료:

- Python 공식 문서: [임시 파일과 디렉토리 생성](https://docs.python.org/3/library/tempfile.html)
- Python 프로그래밍에 대한 자세한 가이드: [임시 파일 사용](https://pymotw.com/3/tempfile/)