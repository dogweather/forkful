---
title:    "Python: 디렉토리 존재 여부 확인"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/python/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 왜?

파이썬 프로그래밍을 하다보면, 종종 어떤 디렉토리가 존재하는지 확인해야 할 때가 있습니다. 이를 확인하는 것은 디렉토리 내의 파일을 찾는 작업이나 프로그램이 유효한 입력 경로를 가져오는 데 유용합니다.

## 어떻게 확인할까요?

먼저, `os` 모듈을 가져온 후 `os.path.exists()` 함수를 사용하여 디렉토리가 실제로 존재하는지 확인할 수 있습니다.

```Python
import os

if os.path.exists("my_directory"): # 이 부분을 디렉토리 이름으로 변경해주세요.
  print("디렉토리가 존재합니다.")
else:
  print("디렉토리가 존재하지 않습니다.")
```

위 코드는 "my_directory"가 존재하는 경우 "디렉토리가 존재합니다."라는 메시지를 출력하고, 존재하지 않는 경우 "디렉토리가 존재하지 않습니다."라는 메시지를 출력합니다.

## 깊게 파고들어봅시다

만약 우리가 디렉토리가 존재하는지 확인하는 대신에, 디렉토리 경로를 가져오길 원한다면 어떻게 해야 할까요? 이 경우에는 `os.path.join()` 함수를 사용하여 경로를 만들어낼 수 있습니다. `os.path.join()` 함수는 입력된 각각의 요소를 디렉토리 구분자(Windows에서는 "\\", MacOS나 Linux에서는 "/")로 연결한 한 개의 값을 반환합니다.

```Python
import os

# 현재 디렉토리
current_dir = os.getcwd()
print(current_dir) # C:/users/username

# "my_directory" 이름의 디렉토리 경로를 만듭니다.
new_dir = os.path.join(current_dir, "my_directory")
print(new_dir) # C:/users/username/my_directory
```

## 참고로

만약 여러분이 디렉토리가 아닌 파일이 존재하는지 확인하고 싶다면 `os.path.isfile()` 함수를 사용하면 됩니다. 또한 디렉토리를 삭제하고 싶다면 `os.rmdir()` 함수를 사용하면 됩니다.

## 더 알아보기

- [Python os 모듈 문서](https://docs.python.org/3/library/os.html)
- [Python os.path 모듈 문서](https://docs.python.org/3/library/os.path.html)