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

## 왜 
누군가가 임시 파일을 만들게 되는 이유는 주로 데이터를 일시적으로 저장하거나 임시 작업을 수행하기 위해서입니다.

## 사용 방법
일시적인 파일을 만드는 데는 `tempfile` 모듈을 사용합니다. 먼저 `import tempfile`을 해준 후, `tempfile.NamedTemporaryFile()` 함수를 호출하면 임시 파일 객체를 반환해줍니다. 예를 들어서 다음과 같은 식으로 사용할 수 있습니다.

```Python
import tempfile

with tempfile.NamedTemporaryFile() as temp_file:
    temp_file.write('Hello, world!')  # 임시 파일에 문자열을 쓰는 예시입니다.
    temp_file.seek(0)  # 파일 포인터를 맨 처음으로 이동해줍니다.
    print(temp_file.read())  # 임시 파일을 읽어옵니다. 출력: b'Hello, world!'
```

## 깊게 들어가기
임시 파일을 만들 때, 운영 체제마다 다르게 작동할 수 있습니다. 일반적으로 파일을 열 때 `mode` 파라미터를 지정해주는데, 이 모드에 따라 파일이 어떻게 사용 가능한지가 달라집니다. 예를 들어 `tempfile.NamedTemporaryFile()`의 기본 모드는 `'w+b'`로, 바이너리 쓰기 모드입니다. 또한, `delete` 파라미터를 `False`로 설정하면 임시 파일이 자동으로 삭제되지 않도록 설정할 수 있습니다. 더 자세한 내용은 [공식 문서](https://docs.python.org/3/library/tempfile.html)를 참고하시기 바랍니다.

## 관련 링크 참고
- [공식 문서](https://docs.python.org/3/library/tempfile.html)
- [Real Python 튜토리얼](https://realpython.com/python-tempfile/)
- [GeeksforGeeks 블로그 포스트](https://www.geeksforgeeks.org/temporary-files-python/)