---
title:    "Python: 부분 문자열 추출"
keywords: ["Python"]
---

{{< edit_this_page >}}

## 왜:

문자열에서 하위 문자열을 추출하는 것은 매우 일반적인 작업입니다. 예를 들어, 문자열을 분석하고 원하는 특정 정보만을 추출하려는 경우에 하위 문자열 추출을 사용할 수 있습니다. 또는 웹 스크레이핑을 한다면 웹 페이지의 특정 부분을 추출하기 위해 하위 문자열 추출을 사용할 수 있습니다. 하위 문자열 추출은 많은 다양한 상황에서 유용하며 문자열 처리를 더욱 쉽게 만들어 줍니다.

## 어떻게:

하위 문자열 추출을 위해 파이썬의 `slice()` 함수를 사용할 수 있습니다. 아래 예시 코드를 통해 살펴보겠습니다.

```Python
# "Hello World!" 문자열에서 "World" 문자열 추출하기
string = "Hello World!"
substring = string[6:11] #인덱스 6부터 11까지 추출 (11은 미포함)
print(substring) #output: World
```

하위 문자열 추출을 위해 사용하는 인덱스는 0부터 시작합니다. 따라서 첫 번째 문자를 추출하려면 `string[0]` 과 같이 사용하면 됩니다. 만약 뒤에서부터 인덱스를 세려면 음수 값을 사용할 수도 있습니다. 예를 들어, `string[-1]`은 문자열의 마지막 문자를 추출하게 됩니다. 또한 인덱스는 생략하여도 됩니다. 생략된 경우, 첫 번째 인덱스 0이 설정되며 마지막 인덱스는 문자열의 끝을 나타내는 기호 `$` 입니다.

## 심층 분석:

파이썬에서는 문자열을 변경할 수 없습니다. 따라서 하위 문자열을 추출하는 것은 복사본을 만드는 작업이라고 생각할 수 있습니다. `slice()` 함수는 복사본을 만들어 지정된 문자열의 일부분을 추출합니다. 하지만 만약 해당 문자열을 이용하지 않고도 원본을 직접 변경하고 싶은 경우, `replace()` 메서드를 사용하면 됩니다. 아래 예시 코드를 통해 살펴보겠습니다.

```Python
string = "Hello World!"
substring = string[6:11] #인덱스 6부터 11까지 추출 (11은 미포함)
print(string) #output: Hello World!
# 위에서 추출한 "World" 문자열을 "Python"으로 변경
string = string.replace(substring, "Python") 
print(string) #output: Hello Python!
```

하위 문자열 추출은 문자열을 다루는 데 매우 유용한 방법 중 하나입니다. 파이썬에서는 `slice()` 함수를 사용하여 쉽게 하위 문자열을 추출할 수 있습니다. 더 깊은 이해를 위해 위의 예시 코드를 바탕으로 다양한 실험을 해보시기 바랍니다.

## 참고:

- [Python Documentation: Text Sequence Type - str](https://docs.python.org/3/library/stdtypes.html#text-sequence-type-str)
- [Python String Methods](https://www.w3schools.com/python/python_ref_string.asp)
  
## 여기를 참고해보세요:

- [우리나라 엘리스 AI 코딩 교육 사이트의 파이썬 기초 강의](https://www.elice.io/courses/python-basic)
- [코딩 도장에서 배우는 파이썬 프로그래밍](https://dojang.io/course/view.php?id=7)