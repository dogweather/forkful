---
title:    "Python: 텍스트 파일 읽기"
keywords: ["Python"]
---

{{< edit_this_page >}}

# 왜?

텍스트 파일을 읽는 것은 파이썬 프로그래밍에 있어서 매우 유용합니다. 텍스트 파일은 데이터를 저장하고 공유하는 매우 일반적이고 효율적인 방법입니다. 따라서 텍스트 파일을 읽는 것은 데이터 처리와 분석을 하는데 중요한 역할을 합니다.

## 어떻게?

```python
# 텍스트 파일을 읽는 방법
file = open("example.txt", "r") # 읽기 모드로 파일 열기
content = file.read() # 파일 내용 읽기
file.close() # 파일 닫기

# 출력 결과
print(content)
```

```python
# 한 줄씩 텍스트 파일을 읽는 방법
file = open("example.txt", "r")
for line in file:
    print(line)

file.close()

# 출력 결과
Line 1
Line 2
Line 3
```

```python
# 파일 내용을 리스트로 저장하는 방법
file = open("example.txt", "r")
content = file.readlines() # 리스트로 파일 내용 읽기
file.close()

# 출력 결과
['Line 1\n', 'Line 2\n', 'Line 3\n']
```

## 깊이 파고들기

텍스트 파일을 읽는 방법은 다양합니다. 예를 들어, `with` 구문을 사용하여 파일을 열고 자동으로 닫을 수 있습니다. 또한 텍스트 파일을 읽을 때 encoding을 지정하여 언어나 형식에 따라 적절하게 읽을 수 있습니다.

텍스트 파일을 읽는 것 이외에도, 파이썬은 `csv` 라이브러리를 통해 csv 파일을 쉽게 읽을 수 있고, `json` 라이브러리를 통해 json 파일을 읽을 수 있습니다. 또한 데이터 분석을 위해 `pandas` 라이브러리를 사용하여 다양한 형식의 파일을 읽고 처리할 수 있습니다.

# 참고 자료

- [Python 공식 문서 - 파일 관리](https://docs.python.org/3.8/tutorial/inputoutput.html#reading-and-writing-files)
- [Real Python - Reading and Writing Files in Python (Guide)](https://realpython.com/read-write-files-python/)
- [GeeksforGeeks - Reading and Writing Files in Python](https://www.geeksforgeeks.org/reading-writing-text-files-python/)
- [Python Pandas - IO tools](https://pandas.pydata.org/docs/reference/io.html)