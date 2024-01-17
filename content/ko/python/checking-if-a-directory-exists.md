---
title:                "디렉토리 존재 여부 확인하기"
html_title:           "Python: 디렉토리 존재 여부 확인하기"
simple_title:         "디렉토리 존재 여부 확인하기"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/python/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 무엇과 왜?

디렉토리가 있는지 확인하는 것은 컴퓨터 프로그래밍에서 중요한 역할을 합니다. 디렉토리가 존재하지 않으면 프로그램이 중단될 수 있으며, 디렉토리의 존재 유무를 확인하는 것은 프로그램의 안정성과 신뢰성을 보장하기 위해서입니다.

## 방법:

```Python
import os

# 디렉토리가 있는지 확인하는 함수
os.path.exists("디렉토리 경로")

# 예제
print(os.path.exists("/Users/Kim/Desktop"))
```

출력결과:

```Python
True
```

## 깊게 살펴보기:

디렉토리가 있는지 확인하는 것은 운영체제의 파일 시스템을 엑세스하기 위해 사용됩니다. 이 함수는 현재 디렉토리, 하위 디렉토리, 파일 등의 존재여부를 확인할 수 있습니다. 또한 이 함수는 경로를 입력받아 특정 디렉토리가 있는지 아닌지를 확인할 수 있습니다.

대안으로는 os.path.isdir() 함수를 사용할 수 있습니다. 이 함수는 입력된 경로가 디렉토리인지 아닌지를 확인하여 참/거짓 값을 반환합니다.

디렉토리가 존재하는지 확인하는 함수는 파일 시스템의 일부로, 운영체제마다 다를 수 있습니다. 따라서, Python 코드가 사용하는 운영체제에 따라 그 결과가 달라질 수 있습니다.

## 관련 자료:

- [The official Python documentation on os.path](https://docs.python.org/3/library/os.path.html)
- [GeeksforGeeks article on checking if a directory exists in Python](https://www.geeksforgeeks.org/python-check-if-a-file-or-directory-exists-2/)