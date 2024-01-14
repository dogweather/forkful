---
title:                "Python: 디렉토리가 존재하는지 확인하기"
simple_title:         "디렉토리가 존재하는지 확인하기"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/python/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 왜

파이썬 프로그래밍을 하다보면 종종 특정한 디렉토리가 존재하는지 확인해야 할 때가 있습니다. 이번 글에서는 왜 디렉토리 존재 여부를 확인하는 것이 중요한지에 대해 알아보겠습니다.

## 어떻게

파이썬에서 디렉토리의 존재 여부를 확인하기 위해서는 `os.path` 모듈에 있는 `exists()` 함수를 사용할 수 있습니다. 이 함수는 해당 경로가 실제로 존재하는지 여부를 불리언 값으로 반환합니다. 예를 들어, 다음과 같은 코드를 실행해보세요.

```Python
import os

# 존재하는 디렉토리 경로
my_dir = '/Users/my_username/Documents'

if os.path.exists(my_dir):
    print('디렉토리가 존재합니다.')
else:
    print('디렉토리가 존재하지 않습니다.')
```

만약 해당 경로에 실제로 디렉토리가 존재한다면 `디렉토리가 존재합니다.`가 출력될 것입니다. 반대로, 디렉토리가 존재하지 않는다면 `디렉토리가 존재하지 않습니다.`가 출력될 것입니다.

## 심층 분석

파이썬에서는 `os.path` 모듈을 사용하여 파일 또는 디렉토리의 존재 여부를 확인할 수 있습니다. 이 모듈에는 다양한 함수들이 있지만, 여기서는 디렉토리 존재 여부를 확인하는 `exists()` 함수에 집중하겠습니다.

`exists()` 함수는 해당 경로에 실제로 존재하는지 여부를 확인해서 불리언 값으로 반환합니다. 따라서 이 함수를 사용하면 파일 또는 디렉토리의 존재 여부를 간단하게 확인할 수 있습니다.

## 관련 자료

- [Python 공식 문서: os.path 모듈](https://docs.python.org/3/library/os.path.html)
- [파이썬 파일 및 디렉토리 관련 기능 알아보기](https://dobest.io/category/Programming/Languages/Python/file-io/)
- [파이썬 파일과 디렉토리 조작하기](http://pythonstudy.xyz/python/article/207-%ED%8C%8C%EC%9D%B4%EC%8D%AC-%EB%AA%A8%EB%93%88%EA%B3%BC-%EB%94%94%EB%A0%89%ED%86%A0%EB%A6%AC-%EC%A1%B0%EC%9E%91%ED%95%98%EA%B8%B0)