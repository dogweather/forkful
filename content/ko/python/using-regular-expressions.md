---
title:                "Python: 정규 표현식 사용하기"
programming_language: "Python"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/python/using-regular-expressions.md"
---

{{< edit_this_page >}}

## 왜

정규 표현식을 사용하는 이유는 간단합니다. 데이터를 분석하고 가공할 때 특정한 패턴을 찾아내는 데 유용하기 때문입니다. 이 패턴은 문자열의 형식이나 패턴을 나타내며, 특정한 규칙을 따르기 때문에 이를 활용하여 데이터를 추출하거나 원하는 형식으로 변경할 수 있습니다.

## 어떻게 사용하나요?

```Python
# 정규 표현식 모듈 불러오기
import re
# 정규 표현식 패턴 설정
pattern = r'\d{3}-\d{3,4}-\d{4}'
# 전화번호 출처 문자열
text = '제 전화번호는 010-1234-5678입니다.'
# 패턴과 일치하는 부분 추출
phone_number = re.search(pattern, text)
# 결과 출력
print(phone_number.group()) # 010-1234-5678
```

정규 표현식에는 다양한 기호와 규칙이 있으며, 문자열에서 원하는 정보를 찾거나 변경하는 데 유용합니다. 또한 정규 표현식을 활용하여 문자열의 특정한 규칙을 검증할 수도 있습니다.

## 더 깊게 알아보기

정규 표현식을 사용하면 데이터를 더욱 효율적으로 분석하고 가공할 수 있습니다. 그러나 복잡한 정규 표현식을 작성하는 것은 쉽지 않은 일일 수 있습니다. 따라서 정규 표현식을 더욱 잘 이해하고 활용하기 위해서는 꾸준한 연습과 공부가 필요합니다. 또한 다양한 패턴과 기호를 익히는 것도 중요합니다.

## 참고 자료

- [정규 표현식 지식인](https://kin.naver.com/qna/detail.nhn?d1id=1&dirId=1050201&docId=307069586&qb=7Jes6rWt7JaRIOyesOyngOyLnCDshZHri50%3D&enc=utf8&section=kin&rank=1&search_sort=0&spq=0)
- [정규 표현식 패턴 메타 문자](https://docs.python.org/3/library/re.html#regular-expression-syntax)
- [정규 표현식 연습 사이트](https://regexr.com/)
- [10분파이썬! 정규 표현식 따라잡기!](https://www.youtube.com/watch?v=j_3Qg3jnrH0)