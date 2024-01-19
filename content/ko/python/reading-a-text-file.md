---
title:                "텍스트 파일 읽기"
html_title:           "Bash: 텍스트 파일 읽기"
simple_title:         "텍스트 파일 읽기"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/python/reading-a-text-file.md"
---

{{< edit_this_page >}}

## 무엇이며 왜?

텍스트 파일 읽기는 데이터를 읽고 분석하는 방법입니다. 실행 결과를 이해하거나 동적으로 데이터를 처리하기 위해 프로그래머는 이를 사용합니다.

## 어떻게:

파이썬에서 텍스트 파일 읽기는 아주 간단합니다. 다음은 파일을 읽어오는 가장 기본적인 코드입니다.

```Python
file = open("testfile.txt", "r") 

for line in file:
    print(line)
    
file.close() 
```
위 코드를 실행하면, "testfile.txt" 안의 내용을 줄 단위로 출력합니다. 파일을 다룰 때 `close()` 메소드를 사용하는 것을 잊지 마세요. 안전한 프로그래밍을 위해 필요합니다.

## 딥 다이브

1. **역사적 맥락**: 파이썬에서의 파일 처리는 언어의 초기 시절부터 가능했습니다. 이는 언어의 유연성과 다양성을 더욱 강조하고 있습니다.

2. **대안들**: 파일을 읽는 다른 방법도 있습니다. 예컨대, `with` 문을 사용하면 파일을 자동으로 닫을 수 있습니다.

```Python
with open("testfile.txt",'r') as file:
    for line in file:
        print(line)
```

3. **구현 세부 내용**: 파이썬에서 `open` 함수는 시스템 레벨에서 I/O 작업을 추상화합니다. 이는 라이브러리나 프레임워크를 사용하지 않고도 텍스트의 읽기 및 쓰기를 가능하게 합니다.

## 참고 자료 

1. 파이썬 파일 I/O: https://docs.python.org/3/tutorial/inputoutput.html#reading-and-writing-files

2. 파이썬 프로그래밍 언어로 파일을 읽고 쓰는 방법의 완전한 튜토리얼: https://www.geeksforgeeks.org/file-handling-python/