---
title:                "Python: 임시 파일 만들기"
programming_language: "Python"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/python/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# 왜

사람들은 종종 프로그램을 작성할 때 일시적인 파일을 만들어야 할 필요가 있습니다. 이는 일시적인 데이터 저장, 파일 경로를 지정하지 않고도 작업을 수행하는 등 다양한 이유로 인해 발생할 수 있습니다. 파이썬에는 이러한 일시적인 파일을 쉽게 만들 수 있는 내장 라이브러리가 있습니다.

# 어떻게

파이썬에서 일시적인 파일을 만드는 것은 매우 쉽습니다. `tempfile` 모듈에서 `TemporaryFile()` 함수를 사용하여 일시적인 파일 객체를 생성할 수 있습니다. 간단한 예제를 살펴보겠습니다.

```Python
# tempfile 모듈 임포트
import tempfile

# temp 파일 생성
temp_file = tempfile.TemporaryFile()

# 파일에 데이터를 작성
temp_file.write(b"Hello, world!")

# 파일 커서를 맨 앞으로 이동
temp_file.seek(0)

# 파일에서 데이터 읽기
data = temp_file.read()

# 출력
print(data)  
```

코드를 실행하면 `Hello, world!`라는 문자열이 출력됩니다. 이는 일시적인 파일을 생성하고 데이터를 작성하고, 커서를 이동시켜 데이터를 읽는 과정을 나타냅니다.

# 깊이 파보기

`tempfile` 모듈에는 `TemporaryFile()` 함수 외에도 다양한 함수와 클래스가 있습니다. 예를 들어, `NamedTemporaryFile()` 함수는 이름이 지정된 일시적인 파일을 생성하고, `TemporaryDirectory()` 함수는 일시적인 디렉토리를 생성합니다. 이 외에도 다양한 옵션이 있으니 관심있는 독자는 공식 파이썬 문서를 참조하면 더 많은 것들을 배울 수 있습니다.

# 관련 글들

- [파이썬 공식 문서 - tempfile 모듈](https://docs.python.org/3/library/tempfile.html)
- [Python Tempfile 모듈을 사용한 일시적인 파일 생성](https://larryning.github.io/python/2018/07/02/python-tempfile)
- [파이썬으로 일시적인 파일 생성하기](https://wikidocs.net/11602)