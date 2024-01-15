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

# 왜

파이썬 프로그래밍은 데이터 처리 및 분석을 위해 많이 사용되는 언어입니다. 우리는 자주 텍스트 파일에서 데이터를 읽어올 필요가 있기 때문에, 텍스트 파일을 읽는 방법에 대해 알아보고자 합니다.

## 어떻게

우선, 텍스트 파일을 읽으려면 open() 함수를 사용해 파일 객체를 생성해야 합니다. 그리고 read() 메소드를 사용하여 파일 내용을 읽을 수 있습니다. 아래는 간단한 예제 코드와 그에 대한 출력 결과입니다.

```Python
f = open("text_file.txt", "r")
file_content = f.read()
print(file_content)
```

```text
Hello world!
This is a sample text file.
We can read and print the contents of this file using Python.
```

파일을 읽을 때, 인코딩 방식을 지정해야 할 수도 있습니다. 예를 들어, UTF-8로 인코딩된 파일을 읽을 때에는 다음과 같이 open() 함수의 두 번째 인자에 "encoding='utf-8'"을 추가해주면 됩니다.

```Python
f = open("text_file.txt", "r", encoding='utf-8')
```

## 깊게 들어가보기

파이썬에서 텍스트 파일을 읽는 것은 실제로 여러 단계로 이루어집니다. open() 함수를 통해 파일 객체를 생성하고, read() 메소드를 사용하여 파일 내용을 읽어오는 것이 우리가 알아본 첫 번째 방법입니다. 그 외에도 readline() 메소드를 사용하여 한 줄씩 읽을 수 있고, readlines() 메소드를 사용하여 모든 줄을 리스트로 읽어올 수도 있습니다.

또한, 파일을 읽는 방식도 다양합니다. 위에서 사용한 "r"은 read-only 모드를 의미합니다. "w"는 쓰기 모드로 파일을 열고, "a"는 append 모드로 파일을 열 수 있습니다. 파일을 읽을 때에는 반드시 파일을 열고 닫아야 하는 것을 잊지 말아야 합니다.

# 봐도 괜찮아요

- [파이썬 공식 문서 - 파일 입출력](https://docs.python.org/3/tutorial/inputoutput.html#reading-and-writing-files)
- [점프 투 파이썬 - 파일 읽고 쓰기](https://wikidocs.net/26)
- [파이썬을 이용해 파일 다루기](https://www.fun-coding.org/PL&OOP1-5.html)