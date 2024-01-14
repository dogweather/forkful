---
title:    "Python: 정규 표현식 사용하기"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/python/using-regular-expressions.md"
---

{{< edit_this_page >}}

## 왜 사용해야 할까요?

정규 표현식은 문자열에서 특정 패턴을 찾거나 대체하는 등 다양한 용도로 사용할 수 있습니다. 이러한 기능들은 프로그래밍에서 매우 유용하며, 특히 큰 데이터셋을 다룰 때 빠르고 정확한 검색을 할 수 있게 해줍니다.

## 사용 방법

정규 표현식은 're' 라이브러리를 사용하여 파이썬 코드에서 쉽게 적용할 수 있습니다. 아래의 코드를 따라해 보세요.

```Python
import re

# 문자열에서 숫자들만 추출하기
text = 'Hello 123 World'
numbers = re.findall('\d+', text)
print(numbers) # ['123']

# 문자열에서 자음들만 추출하기
text = 'Hello 123 World'
consonants = re.findall('[^aeiouAEIOU]+', text)
print(consonants) # ['Hll', ' ' Wrld']
```

## 깊게 파보기

정규 표현식을 적용할 때, 특수 문자에 대하여 적절하게 이스케이프해야 할 필요가 있습니다. 예를 들어, '.' 문자는 모든 문자를 나타내는 메타 문자이기 때문에, 실제 '.' 문자를 찾기 위해서는 '\.' 로 이스케이프 해주어야 합니다.

```Python
# 웹사이트 주소에서 도메인 이름 추출하기
url = 'https://github.com'
domain = re.findall('https:\/\/(.+)\.(.+)', url)
print(domain) # [('github', 'com')]
```

## 관련 링크들

* [정규 표현식 문서 - Python.org](https://docs.python.org/3/library/re.html)
* [정규 표현식 테스트 사이트 - regex101.com](https://regex101.com/)