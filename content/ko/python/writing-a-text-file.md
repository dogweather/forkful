---
title:                "Python: 텍스트 파일 작성하기"
programming_language: "Python"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/python/writing-a-text-file.md"
---

{{< edit_this_page >}}

## 왜

텍스트 파일을 작성하는 것의 중요성을 간단하게 설명한다.

텍스트 파일을 작성하는 것은 프로그래밍에서 가장 기본적이지만 중요한 원리 중 하나입니다. 텍스트 파일을 사용하면 프로그램의 데이터를 저장하고, 불러오고, 공유할 수 있습니다. 이를 통해 코드의 유연성과 확장성을 높일 수 있습니다.

## 어떻게

아래는 파이썬을 사용하여 텍스트 파일을 작성하는 간단한 예제 코드입니다. 코드 블록 외부에는 파일 이름과 모드를 지정하는 부분이 있고, 코드 블록 내부에는 텍스트 파일을 작성하는 과정이 담겨 있습니다.

```Python
# 파일명과 모드 지정
file_name = "sample.txt"
mode = "w"

# 텍스트 파일 작성
with open(file_name, mode) as f:

  # 텍스트 내용 작성
  f.write("안녕하세요!")
  f.write("파이썬을 배우는 것은 정말 즐겁습니다.")

# 작성된 파일 열기
with open(file_name) as f:

  # 파일 내용 출력
  contents = f.read()
  print(contents)

# 출력
안녕하세요!
파이썬을 배우는 것은 정말 즐겁습니다.
```

위 코드를 실행하면 "sample.txt"라는 파일이 생성되고, 해당 파일에는 "안녕하세요!"와 "파이썬을 배우는 것은 정말 즐겁습니다."라는 내용이 작성됩니다. 마지막으로, 작성된 파일을 열어서 내용을 확인할 수 있습니다.

## 딥 다이브

텍스트 파일을 작성할 때 중요한 몇 가지 사항이 있습니다. 첫째는 모드(mode)를 지정하는 것입니다. 위 예제에서는 "w"라는 모드를 사용했는데, 이는 파일을 쓰기(write) 모드로 열겠다는 의미입니다. 파일을 읽기(read) 모드로 열고 싶다면 "r"을, 읽기와 쓰기 모드로 열고 싶다면 "a"를 사용하면 됩니다.

둘째로는 파일을 닫는 것을 잊지 않는 것입니다. 위 코드에서는 `with`문을 사용하여 파일을 자동으로 닫아주기 때문에 따로 `f.close()`를 호출하지 않아도 됩니다.

마지막으로, 텍스트 파일을 작성할 때에는 인코딩(encoding)을 고려해야 합니다. 기본적으로 파이썬은 UTF-8 인코딩을 사용하기 때문에 대부분의 경우에는 따로 설정해 줄 필요는 없지만, 특정 언어의 특수 문자를 사용하거나 다른 인코딩을 적용해야 할 경우에는 인코딩을 지정해 주어야 합니다.

## 관련 링크

- [파이썬 공식 문서 - 파일 입출력](https://docs.python.org/3/tutorial/inputoutput.html#reading-and-writing-files)
- [programiz.com - 파일 입출력](https://www.programiz.com/python-programming/file-operation)
- [tutorials point - 텍스트 파일 작성하기](https://www.tutorialspoint.com/python/python_files_io.htm)