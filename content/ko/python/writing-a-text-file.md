---
title:    "Python: 텍스트 파일 작성하기"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/python/writing-a-text-file.md"
---

{{< edit_this_page >}}

## 왜
텍스트 파일을 작성하는 이유는 프로그램에서 중요한 데이터를 저장하고 불러오는 데 사용하기 위해서입니다.

## 작성하는 방법
우선, 텍스트 파일을 작성하기 위해서는 작성할 파일의 이름과 경로를 지정해야 합니다. 그 후에는 `open()` 함수를 사용하여 파일을 열고, `write()` 함수를 사용하여 파일 안에 내용을 작성할 수 있습니다. 코드 블록 안에 다음과 같이 작성해 보세요.

```python
# 파일 열기
file = open("my_file.txt", "w")

# 텍스트 파일 작성
file.write("안녕하세요! 이것은 샘플 텍스트 파일입니다!")

# 파일 닫기
file.close()
```

위의 코드를 실행하면, 파일 시스템에 `my_file.txt`라는 이름의 텍스트 파일이 생성되고 내용으로 "안녕하세요! 이것은 샘플 텍스트 파일입니다!"라는 문구가 쓰여집니다.

## 깊이 파헤치기
만약 텍스트 파일 안에 여러 줄의 내용을 작성하고 싶다면, `write()` 함수에 `\n`을 추가하여 개행을 해 주면 됩니다. 또한, `write()` 함수 대신 `writelines()` 함수를 사용하여 여러 줄의 내용을 한 번에 작성할 수도 있습니다. 코드 블록 안에 다음과 같이 작성해 보세요.

```python
# 파일 열기
file = open("my_file.txt", "w")

# 여러 줄의 텍스트 파일 작성
lines = ["첫 번째 줄\n", "두 번째 줄\n", "세 번째 줄\n"]
file.writelines(lines)

# 파일 닫기
file.close()
```

위의 코드를 실행하면, `my_file.txt`라는 이름의 텍스트 파일이 생성되고 내용으로 각 줄마다 "첫 번째 줄", "두 번째 줄", "세 번째 줄"이 작성됩니다.

## 이어보기
이외에도 `read(), readline(), readlines()` 함수를 사용하여 텍스트 파일 안의 내용을 읽어올 수 있고, `seek()` 함수를 사용하여 파일의 특정 위치로 이동할 수도 있습니다. 자세한 내용은 관련 문서를 참고하시기 바랍니다.

## 관련 문서
- [파이썬 파일 입출력 문서](https://docs.python.org/ko/3/tutorial/inputoutput.html#reading-and-writing-files)