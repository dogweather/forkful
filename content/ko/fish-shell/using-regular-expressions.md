---
title:                "정규 표현식 사용하기"
html_title:           "Fish Shell: 정규 표현식 사용하기"
simple_title:         "정규 표현식 사용하기"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/fish-shell/using-regular-expressions.md"
---

{{< edit_this_page >}}

## 어떤거고 왜 하는 거야?
정규표현식을 사용하는 것은 무엇인지 알려주고, 프로그래머들이 이것을 왜 사용하는지 설명해 줄게.

정규표현식은 문자열에서 특정한 패턴을 찾아내는 데 사용되는 문자열 검색 도구야. 우리가 필요한 정보를 쉽게 추출하거나, 데이터를 정제하거나, 유효성 검사를 할 때 유용하게 쓰이지. 프로그래밍 작업을 더욱 효율적으로 하기 위해 정규표현식을 사용하는 경우가 많아.

## 어떻게 하는거야?
Fish Shell의 정규표현식을 사용하는 예제와 그 결과를 ```Fish Shell ... ```코드 블록 안에서 보여줄게.

### 이메일 주소 추출하기
`echo "제 이메일 주소는 example@example.com입니다." | fish -c "string match -r '([a-z0-9_\.-]+)@([\da-z\.-]+)\.([a-z\.]{2,6})' >> 이메일 주소 추출 결과.txt"`

`cat 이메일 주소 추출 결과.txt`
```
example@example.com
```

### 숫자만 추출하기
`echo "판매량: 3456개" | fish -c "string match -r '([0-9]+)' >> 숫자 추출 결과.txt"`

`cat 숫자 추출 결과.txt`
```
3456
```

## 깊게 파고들기
정규표현식은 켄 톰슨이 개발한 텍스트 처리 도구인 ed의 일부기능으로 1950년대에 처음 사용되었어. 현재까지도 널리 사용되지만, 다른 문자열 처리 방법과 비교하면 익숙하지 않은 문법이라 사용하기 어려울 수도 있어.

또한, 정규표현식 대신 문자열 처리 라이브러리를 사용하는 방법도 있어. 하지만 정규표현식은 여러 패턴을 한 번에 검색할 수 있다는 장점이 있어서 가독성이나 성능 면에서 더 좋은 선택일 수 있어.

Fish Shell의 정규표현식은 C 언어의 정규표현식을 기반으로한 자체 구현체를 사용해. 따라서 C 언어의 정규표현식 문법을 그대로 적용할 수 있어서 익숙한 사용이 가능해.

## 관련 자료
- [Fish Shell 공식 홈페이지](https://fishshell.com/)
- [Fish Shell GitHub 레포지토리](https://github.com/fish-shell/fish-shell)
- [C 언어 정규표현식 문법](https://www.gnu.org/software/gawk/manual/gawk.html#Regexp-Operators)