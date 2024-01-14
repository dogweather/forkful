---
title:    "Python: 디렉토리가 존재하는지 확인하기"
keywords: ["Python"]
---

{{< edit_this_page >}}

## 왜 

파이썬 프로그래밍을 하는 이유는 다양할 수 있지만, 이 글에서는 디렉토리가 존재하는지 확인하는 방법에 대해 알아보겠습니다. 디렉토리가 존재하는지 여부는 프로그램을 작성할 때 중요한 요소 중 하나이기 때문에 반드시 알아두어야 합니다.

## 어떻게 하나요?

```python
import os

if os.path.exists("directory_name"):
    print("디렉토리가 존재합니다.")
else:
    print("디렉토리가 존재하지 않습니다.")
```

위의 예시 코드를 확인해보면, 우선 `os` 모듈을 `import`하여 디렉토리에 대한 정보를 가져옵니다. 그리고 `path.exists()` 함수를 이용하여 디렉토리가 존재하는지 여부를 확인합니다. 만약 디렉토리가 존재한다면 "디렉토리가 존재합니다."라는 메시지를 출력하고, 그렇지 않다면 "디렉토리가 존재하지 않습니다."라는 메시지를 출력합니다.

## 깊이 들어가기

파이썬에서 디렉토리가 존재하는지 확인하는 방법은 다양하지만, `os.path.exists()` 함수를 이용하는 것이 가장 간단하고 효율적입니다. 이 함수는 디렉토리 뿐만 아니라 파일의 존재 여부도 확인할 수 있기 때문에 매우 유용합니다.

디렉토리의 경우, 해당 디렉토리가 없을 경우에는 `False`를 반환하고, 디렉토리가 존재할 경우에는 `True`를 반환합니다. 따라서 `if`문을 사용하여 `True`일 경우와 `False`일 경우에 다른 동작을 할 수 있습니다.

`os.path.exists()` 함수 외에도 `os.path.isdir()` 함수를 이용하여 디렉토리인지 아닌지를 확인할 수 있습니다. 파일인지 아닌지는 `os.path.isfile()` 함수를 통해 확인할 수 있습니다.

## 참고자료

- [Python 공식 문서: os.path - 경로 관리](https://docs.python.org/ko/3/library/os.path.html)
- [파이썬 코딩 알고리즘 도감](https://wikidocs.net/4308) 
- [Python 기본기 다지기](https://itholic.github.io/python-basic/)