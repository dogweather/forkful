---
title:                "텍스트 파일 읽기"
html_title:           "Python: 텍스트 파일 읽기"
simple_title:         "텍스트 파일 읽기"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/python/reading-a-text-file.md"
---

{{< edit_this_page >}}

## 무엇이며 왜?: 
텍스트 파일을 읽는 것은 파일 안에 있는 텍스트 데이터를 읽어오는 것을 말합니다. 프로그래머들이 이 작업을 하는 이유는 해당 파일 안에 있는 정보를 사용하기 위해서입니다.

## 어떻게 하나요?:
```python 
# 파일 열기
file = open("example.txt", "r")

# 파일 내용 읽기
data = file.read()

# 파일 닫기
file.close()

# 출력하기
print(data)
```

## 더 깊게 들어가보면: 
텍스트 파일을 읽는 방법은 예전부터 사용되어 왔으며, 중요한 정보를 담고 있는 파일을 읽기 위한 대표적인 방법입니다. 다른 방법으로는 바이너리 파일을 읽는 방법도 있지만, 텍스트 파일은 읽기 쉽고 이해하기 쉬우며, 프로그래머들뿐만 아니라 모든 사람이 쉽게 읽고 이해할 수 있습니다. 또한, 파이썬에서는 내장 함수인 `open()`을 사용하여 간단하게 파일을 열고, `read()`로 파일 내용을 읽을 수 있습니다.

## 관련 자료: 
- [파이썬 입출력 함수](https://docs.python.org/ko/3/tutorial/inputoutput.html)
- [Python File Handling](https://www.geeksforgeeks.org/file-handling-python/)
- [파이썬 파일 읽고 쓰기](https://wikidocs.net/26)