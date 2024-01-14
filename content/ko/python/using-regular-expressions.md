---
title:    "Python: 정규 표현식 사용하기"
keywords: ["Python"]
---

{{< edit_this_page >}}

## 왜

정규식을 사용하는 이유는 문자열에서 특정 패턴을 찾아내기 위해서입니다.

## 사용법

파이썬에서는 `re` 모듈을 사용하여 정규식을 쉽게 다룰 수 있습니다. 예를 들어, 다음과 같은 텍스트가 있다고 가정해봅시다.

```
text = "제 이메일 주소는 example1234@gmail.com입니다."
```

우리는 이메일 주소를 찾아내기 위해 다음과 같이 작성할 수 있습니다.

```
import re

pattern = r"[\w.]+@[\w.]+\.\w+"
# [\w.]+: 알파벳, 숫자, 점을 포함한 모든 문자열을 의미합니다.
# @: @ 기호를 의미합니다.
# \w+: 알파벳, 숫자를 포함한 모든 문자열을 의미합니다.
# \.: . 기호를 의미합니다.
# \w+: 알파벳, 숫자를 포함한 모든 문자열을 의미합니다.

result = re.findall(pattern, text)
print(result)
# 결과: ['example1234@gmail'']
```

위 코드를 보면, `re` 모듈의 `findall()` 함수를 사용해서 정규식을 적용한 것을 볼 수 있습니다. `findall()` 함수는 매칭되는 모든 부분을 리스트로 반환합니다. 이처럼 정규식을 사용하면 원하는 패턴을 손쉽게 찾을 수 있습니다.

## 깊이 있는 정보

정규식을 사용할 때 주의해야 할 점은 패턴을 정확하게 작성하는 것입니다. 만약 패턴이 완전하지 않다면 원하는 결과를 얻지 못할 수 있습니다. 또한, 정규식을 사용하면 코드의 가독성이 떨어지기 때문에 주석을 적극적으로 사용하는 것이 좋습니다. 더 자세한 정보는 다음 링크를 참고해주세요.

- [정규식 입문](https://wikidocs.net/1642)
- [정규식 튜토리얼](https://regexone.com/lesson/introduction_abcs)
- [정규식 자습서](https://www.geeksforgeeks.org/regular-expression-python/)

## 또 다른 것들

- [파이썬 문서](https://docs.python.org/ko/3/howto/regex.html)
- [정규식 테스트 도구](https://regexr.com/)