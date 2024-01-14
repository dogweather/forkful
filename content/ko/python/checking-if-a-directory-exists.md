---
title:                "Python: 디렉토리가 존재하는지 확인하기"
programming_language: "Python"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/python/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 왜

디렉토리가 존재하는지 확인하는 것이 중요한 이유는, 우리가 코드를 작성할 때 해당 디렉토리가 있는지 확인하고, 없으면 새로 생성하기 위해서 입니다.

## 어떻게 할까요

```Python
import os

# 디렉토리가 있는지 확인하는 함수
def check_directory(directory):
    if os.path.exists(directory): # 디렉토리가 존재하는지 확인
        print("디렉토리가 이미 존재합니다.")
    else:
        os.makedirs(directory) # 디렉토리 생성
        print("디렉토리가 생성되었습니다.")

# 디렉토리 이름 입력 받기
directory_name = input("디렉토리 이름을 입력하세요: ")

# 함수 호출
check_directory(directory_name)
```
위의 코드는 입력받은 디렉토리 이름을 확인하고, 존재하지 않는다면 새로 생성하는 간단한 예시입니다.

## 깊게 알아보기

이제 좀 더 깊숙한 곳으로 들어가서 디렉토리가 존재하는지 확인하는 패키지를 살펴보겠습니다. os 모듈의 `os.path`는 경로 관련 여러 함수들을 제공하며, `os.path.exists()` 함수는 해당 경로에 파일이나 디렉토리가 있는지 확인합니다. 또한 `os.makedirs()` 함수는 지정한 경로에 디렉토리를 생성합니다.

## 또한 보기

- [Python 공식 문서 - os.path 모듈](https://docs.python.org/3/library/os.path.html)
- [파이썬 코딩 도장 - 디렉토리 만들기](https://dojang.io/mod/page/view.php?id=2363)