---
title:    "Python: 텍스트 파일 읽기"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/python/reading-a-text-file.md"
---

{{< edit_this_page >}}

# 왜

파일을 읽는 것이 왜 중요한지 궁금하셨나요? 파일을 읽는 것은 컴퓨터 프로그래밍의 기초 중 하나입니다. 파일은 다양한 유형의 데이터를 저장할 수 있기 때문에, 파이썬을 포함한 다양한 프로그래밍 언어에서 파일을 읽는 방법을 배우는 것은 매우 유용합니다.

# 어떻게 하나요?

파이썬에서 파일을 읽는 것은 매우 간단합니다. 먼저 파일을 읽고자 하는 경로를 지정해야 합니다. 그 후, `open()` 함수를 사용하여 파일을 열고, `read()` 함수를 사용하여 파일을 읽을 수 있습니다. 아래 예시를 살펴보세요.

```Python
file_path = "text_file.txt" # 파일 경로 설정
file = open(file_path) # 파일 열기
text_data = file.read() # 파일 읽기
print(text_data) # 파일 내용 출력
```

위 코드의 결과는 파일 `text_file.txt`의 내용을 출력할 것입니다. 또 다른 방법으로는, `with` 구문을 사용하여 파일을 열고, `readlines()` 함수를 사용하여 파일의 각 줄을 읽을 수 있습니다. 아래 예시를 살펴보세요.

```Python
file_path = "text_file.txt" # 파일 경로 설정
with open(file_path) as file: # 파일 열기
    lines = file.readlines() # 파일의 각 줄 읽기
    for line in lines: # 각 줄 출력
        print(line)
```

# 깊이 파고들기

파일을 읽는 것은 항상 간단하지는 않습니다. 파일에 대한 추가적인 작업이 필요할 수도 있습니다. 예를 들어, 파일 내의 데이터를 분석하거나 원하는 특정 부분을 추출하고 싶을 수 있습니다. 이를 위해 정규표현식을 사용하는 방법 등 파일을 읽는 것에 대한 더 많은 정보를 알아볼 수 있습니다. 또 파일을 읽는 것에서 발생할 수 있는 오류에 대해 알아봄으로써 더 강력한 코드를 작성할 수도 있습니다.

# 관련 링크

[파이썬 입문자를 위한 파일 입출력](https://wikidocs.net/26)

[파이썬으로 파일 다루기](https://codeit.kr/learn/courses/python-intermediate/files)

[정규표현식으로 파일 다루기](https://www.datacamp.com/community/tutorials/python-regular-expression-tutorial)