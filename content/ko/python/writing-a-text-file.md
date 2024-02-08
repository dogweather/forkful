---
title:                "텍스트 파일 쓰기"
date:                  2024-02-03T19:29:14.392285-07:00
model:                 gpt-4-0125-preview
simple_title:         "텍스트 파일 쓰기"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/python/writing-a-text-file.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 무엇과 왜?
Python에서 텍스트 파일 쓰기는 파일을 생성하거나 열고 텍스트를 추가하거나 덮어쓰는 것을 포함하는 기본적인 작업입니다. 이 기능은 데이터 로깅, 구성 관리 및 프로그램에 의해 생성된 출력의 저장에 필수적이어서 프로그래머의 기본적이면서도 필수적인 도구로 만듭니다.

## 방법:
### 내장된 `open()` 함수 사용하기
Python의 내장 `open()` 함수는 파일에 쓰는 가장 일반적인 방법입니다. 이 함수는 파일을 열 때 지정하는 모드 - 'w'는 쓰기(덮어쓰기), 'a'는 추가, 그리고 'w+'는 쓰기+읽기입니다.

```python
# 새 파일에 쓰기 또는 기존 파일 교체하기
with open('example.txt', 'w') as file:
    file.write("Hello, World!\n")

# 파일에 추가하기
with open('example.txt', 'a') as file:
    file.write("Appending more text.\n")

# 파일을 읽어서 확인하기
with open('example.txt', 'r') as file:
    print(file.read())
```
**출력 예시:**
```
Hello, World!
Appending more text.
```
### `pathlib.Path` 사용하기
더 객체 지향적인 접근 방식을 위해, `pathlib` 모듈의 `Path` 클래스는 파일에 쓰는 메서드를 제공합니다. 이는 새로운 Python 코드베이스에서 인기 있는 메서드입니다.

```python
from pathlib import Path

# 파일을 쓰거나 교체하기
Path('example2.txt').write_text("This is example 2.\n")

# 파일을 읽어서 확인하기
print(Path('example2.txt').read_text())

# 참고: `Path.write_text`는 항상 파일 내용을 덮어씁니다.
# 추가하기 위해서는 이전 섹션에서 보여준 것처럼 파일을 열어야 합니다.
```
**출력 예시:**
```
This is example 2.
```

### 서드파티 라이브러리
복잡한 파일 작업을 위해 `pandas`(CSV, Excel 파일용)와 같은 서드파티 라이브러리는 좋은 자산이 될 수 있습니다. 여기 `pandas`를 사용하여 DataFrame을 CSV 파일에 쓰는 간단한 예시가 있어 단순한 텍스트 파일을 넘어서는 유용함을 보여줍니다.

```python
# 이 예시는 pandas가 필요합니다: pip install pandas
import pandas as pd

# 간단한 DataFrame 생성하기
data = pd.DataFrame({'Column1': [1, 2, 3], 'Column2': ['A', 'B', 'C']})

# DataFrame을 CSV 파일에 쓰기
data.to_csv('example.csv', index=False)

# CSV를 읽어서 확인하기
print(pd.read_csv('example.csv'))
```
**출력 예시:**
```
   Column1 Column2
0        1       A
1        2       B
2        3       C
```

이러한 방법들을 사용하여 Python 프로그래머는 간단하고 복잡한 데이터 처리 필요성에 모두 대응할 수 있는 파일 작업을 효과적으로 관리할 수 있습니다.
