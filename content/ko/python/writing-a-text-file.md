---
title:                "텍스트 파일 작성하기"
html_title:           "Arduino: 텍스트 파일 작성하기"
simple_title:         "텍스트 파일 작성하기"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/python/writing-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
텍스트 파일 작성은 문자 데이터를 파일로 저장하는 행위입니다. 이는 데이터 로깅, 설정 저장, 사용자 데이터 출력 등 다양한 이유로 프로그래머들이 사용합니다.

## How to: (어떻게:)
```Python
# 파일 쓰기 예제

# 'w' 모드로 파일을 연다 (기존 내용은 삭제)
with open('example.txt', 'w', encoding='utf-8') as file:
    file.write("파이썬으로 텍스트 파일 쓰기.\n")
    file.write("간단하고 즐겁습니다!")

# 파일 읽기 및 출력 예제
with open('example.txt', 'r', encoding='utf-8') as file:
    content = file.read()
    print(content)

# 예상 출력:
# 파이썬으로 텍스트 파일 쓰기.
# 간단하고 즐겁습니다!
```

## Deep Dive (심화 학습)
텍스트 파일 작성은 컴퓨터의 초기부터 사용되어 왔습니다. `open` 함수 대신에 `io` 모듈이나 다른 언어의 파일 처리 API로도 같은 작업을 할 수 있습니다. 텍스트 파일 작성 시 인코딩은 중요한데, 파이썬 3.x 버전에서는 기본적으로 'utf-8'을 사용합니다. 대용량 데이터를 처리할 때는 메모리 관리를 위해 파일을 분할하거나, 버퍼링 기법을 사용하기도 합니다.

## See Also (더 보기)
- [파이썬 공식 문서 파일 I/O](https://docs.python.org/3/tutorial/inputoutput.html#reading-and-writing-files)
- [W3Schools Python File Handling](https://www.w3schools.com/python/python_file_handling.asp)
- [Real Python - Working with Files in Python](https://realpython.com/working-with-files-in-python/)