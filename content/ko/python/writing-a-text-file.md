---
title:                "텍스트 파일 작성하기"
html_title:           "Python: 텍스트 파일 작성하기"
simple_title:         "텍스트 파일 작성하기"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/python/writing-a-text-file.md"
---

{{< edit_this_page >}}

## 왜

텍스트 파일을 작성하는 것이 왜 중요한지 궁금하지 않습니까? 컴퓨터에 저장된 데이터를 효율적으로 관리하고 저장하는 것은 프로그래밍에서 중요한 부분이며 텍스트 파일은 이를 가능하게 해줍니다.

## 작성하는 방법

```Python
# 예시 코드 
# 다음은 "example.txt" 파일을 작성하는 코드입니다. 
with open("example.txt", "w") as file:
    file.write("이것은 예시 텍스트입니다.")
```

위의 코드는 해당 디렉토리에 "example.txt" 파일을 생성하고 그 안에 "이것은 예시 텍스트입니다."라는 내용을 쓰는 방법을 보여줍니다.

텍스트 파일을 작성하기 위해서는 다음과 같은 단계를 따르면 됩니다.

- `open()` 함수를 사용하여 파일을 열고 쓰기 모드로 지정합니다.
- `write()` 함수를 사용하여 원하는 내용을 파일에 씁니다.
- 작업이 끝나면 `close()` 함수를 사용하여 파일을 닫습니다.

추가적으로, `with` 구문을 사용하면 코드를 간결하고 보다 안전하게 작성할 수 있습니다.

## 깊게 파보기

텍스트 파일을 작성할 때는 다양한 옵션들을 설정할 수 있습니다. `open()` 함수에서 `mode` 매개변수를 사용하면 파일을 여는 모드를 지정할 수 있습니다. 기본값은 "w"로 쓰기 모드입니다. 만약 파일이 이미 존재한다면 기존 내용이 덮어씌워지므로 주의해야 합니다.

또한 파일을 작성할 때, 인코딩 옵션을 설정할 수 있습니다. 기본적으로 UTF-8 인코딩이 사용되지만 다른 인코딩을 사용하고 싶다면 `encoding` 매개변수를 사용하여 설정할 수 있습니다.

마지막으로, 텍스트 파일에는 여러 줄의 내용을 한번에 쓸 수 있습니다. `write()` 함수를 여러번 사용하면 됩니다.

## 더 알아보기

파일을 작성하는 것 외에도, 텍스트 파일을 읽고 수정하는 것도 중요합니다. 이를 위해 `open()` 함수에서 "r" 모드를 사용하면 파일을 읽기 모드로 열 수 있습니다.

그리고 `os` 모듈을 사용하면 파일을 생성하고 삭제하는 등의 다양한 파일 관리 작업을 할 수 있습니다.

## 관련 링크

- [파이썬 공식 문서](https://docs.python.org/ko/3/tutorial/inputoutput.html#reading-and-writing-files)
- [Real Python 블로그](https://realpython.com/read-write-files-python/)
- [GeeksforGeeks 튜토리얼](https://www.geeksforgeeks.org/reading-writing-text-files-python/)