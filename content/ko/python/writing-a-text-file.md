---
title:                "Python: 텍스트 파일 쓰기"
simple_title:         "텍스트 파일 쓰기"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/python/writing-a-text-file.md"
---

{{< edit_this_page >}}

Python 프로그래밍 블로그 포스트 작성에 관한 친자막

## 왜

파일을 작성하는 이유는 데이터를 저장하고 나누기 위해서입니다. 텍스트 파일은 데이터를 구조화하는 가장 간단한 방법 중 하나입니다.

## 작성하는 방법

텍스트 파일을 작성하는 것은 매우 간단한 작업입니다. 우선, "file.txt"와 같이 파일 이름을 정하고, 쓰기 모드로 열어줍니다. 그리고 "write()" 함수를 사용하여 내용을 적어줍니다. 마지막으로 "close()" 함수를 사용하여 파일을 닫아줍니다.

```Python
file = open("file.txt", "w")
file.write("This is a sample text file.")
file.close()
```

파일을 열 때 "w" 외에도 "a" (추가)나 "r" (읽기)와 같은 모드를 선택할 수 있습니다. 또한 "with" 문을 사용하여 파일을 열고 작업이 끝나면 자동으로 파일을 닫을 수도 있습니다.

```Python
with open("file.txt", "w") as file:
    file.write("This is a sample text file.")
```

## 깊이 파헤치기

파일을 작성하는 것만으로는 충분하지 않을 수 있습니다. 때때로 파일을 구조적으로 작성하는 것이 더 중요할 때도 있습니다. 예를 들어, CSV 파일을 작성한다면 문자열을 쉼표로 구분하여 작성해야 합니다.

```Python
with open("file.csv", "w") as file:
    file.write("Name,Age,Country\n")
    file.write("John,25,USA\n")
    file.write("Maria,30,Canada\n")
```

파일을 작성하는 과정에서 다양한 파이썬 내장 함수를 사용할 수도 있습니다. 예를 들어 "str()" 함수를 사용하여 정수나 실수를 문자열로 변환한 뒤 파일에 쓸 수 있습니다.

```Python
with open("file.txt", "w") as file:
    file.write(str(123))
```

## 참고 자료

- [파이썬 파일 입출력 - W3Schools (한국어)](https://www.w3schools.com/python/python_file_write.asp)
- [10 Minutes to Pandas - Writing to a csv file](https://pandas.pydata.org/pandas-docs/stable/getting_started/10min.html#writing)
- [Real Python - Working With Files in Python](https://realpython.com/working-with-files-in-python/)
- [파이썬 공식 문서 - 파일 객체](https://docs.python.org/ko/3/tutorial/inputoutput.html#reading-and-writing-files)