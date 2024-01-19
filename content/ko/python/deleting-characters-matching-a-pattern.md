---
title:                "패턴에 일치하는 문자 삭제"
html_title:           "Fish Shell: 패턴에 일치하는 문자 삭제"
simple_title:         "패턴에 일치하는 문자 삭제"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/python/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## 무엇이고 왜 사용하는가?
문자열에서 패턴과 일치하는 문자를 삭제하는 기능은 주어진 문자열에서 특정 문자나 문자 패턴을 제거하는 Python 메서드입니다. 이러한 처리는 원하지 않는 문자를 제거하거나 문자열을 정리하고 검색을 용이하게 하기 위해 프로그래머들이 종종 사용합니다.

## 사용 방법
Python에서 문자 패턴을 삭제하는데는 여러 방법이 있습니다. 가장 간단한 방법은 `replace`함수를 사용하는 것입니다.

```python
# 문자열에서 특정 문자 삭제
text = "안녕, 여러분!"
clean_text = text.replace("!", "")
print(clean_text) # "안녕, 여러분" 출력
```

`translate`와 `maketrans` 함수를 사용하여 여러 문자를 삭제할 수도 있습니다.

```python
text = "안녕, 여러분!!"
remove_chars = "!,"
table = str.maketrans("", "", remove_chars)
clean_text = text.translate(table)
print(clean_text) # "안녕 여러분" 출력
```

## 깊게 알아보기
문자 패턴을 삭제하는 기능은 뽑아내야하는 데이터가 문자열에 묶여있다는 데서부터 비롯된 것입니다. 처음에는이 문제를 해결하기 위해 문자열을 수동으로 분석하고 삭제해야 했습니다.

다양한 알고리즘을 사용하여 동일한 결과를 얻을 수 있지만, `replace`와 `translate`는 Python에서 가장 효율적인 메서드들 중에 하나입니다. 사실 `replace`는 전체 문자열을 검색하고 `translate`는 한 번에 반복하며 각 문자를 검사합니다.

`replace`와 `translate`의 선택은 상황에 따라 달라집니다. `replace`는 단순하고 읽기 쉬우며 단어와 같은 긴 문자열을 제거하는 데 좋습니다. 반면에 `translate`는 단일 문자를 제거하는데 효과적입니다.

## 참고 자료
자세한 내용을 보려면 아래 링크로 이동하세요.

-Python 공식 문서 ([`replace`](https://docs.python.org/3/library/stdtypes.html#str.replace), [`translate`](https://docs.python.org/3/library/stdtypes.html#str.translate), [`maketrans`](https://docs.python.org/3/library/stdtypes.html#str.maketrans))

-Stack Overflow ([Removing certain characters from a string](https://stackoverflow.com/questions/3939361/remove-specific-characters-from-a-string-in-python))