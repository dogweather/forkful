---
title:                "정규식 사용하기"
html_title:           "Python: 정규식 사용하기"
simple_title:         "정규식 사용하기"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/python/using-regular-expressions.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

정규 표현식 사용이 무엇인지 설명하고, 프로그래머들이 왜 이를 사용하는지에 대해 두 문장 정도로 알아보겠습니다.

## 어떻게:

파이썬 코드 블록 `Python ...` 안에 코딩 예제와 샘플 출력을 포함하겠습니다.

```Python
# 이메일 유효성 검사하기
import re

email = input("이메일 주소를 입력하세요: ")

if re.match("[a-z0-9]+@[a-z]+\.[a-z]+", email):
    print("올바른 이메일 형식입니다.")
else:
    print("잘못된 이메일 형식입니다.")
```

## 심층 분석:

1. 역사적 맥락: 정규 표현식은 1950년대에 미국 수학자인 스티븐 콜레이가 제안했으며, 프로그래밍 언어에서 문자열을 처리하는 과정에서 많은 도움을 주었습니다.
2. 대안: 정규 표현식보다 간단한 방법으로 문자열을 처리할 수 있는 방법도 있지만, 정규 표현식은 더욱 강력하고 다양한 기능을 제공합니다.
3. 구현 세부 사항: 파이썬에서 `re` 모듈을 import 하여 정규 표현식을 사용할 수 있으며, 단순한 검색 뿐만 아니라 문자열 치환 등 다양한 기능을 제공합니다.

## 관련 자료:

- [Python 공식 문서: re 모듈](https://docs.python.org/3/library/re.html)
- [점프 투 파이썬: 정규 표현식 교과서](https://wikidocs.net/book/2155)