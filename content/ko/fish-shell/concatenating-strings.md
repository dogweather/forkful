---
title:    "Fish Shell: 문자열 연결하기"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/fish-shell/concatenating-strings.md"
---

{{< edit_this_page >}}

## 왜

문자열을 연결하는 것에 참여하는 이유가 무엇일까요? 그 이유는 문자열을 조합하여 더 큰 문자열을 생성할 수 있기 때문입니다. 이를 통해 여러 문자열을 하나의 변수로 조작하거나 출력에 사용할 수 있습니다.

## 방법

먼저, 문자열 연결의 기본적인 개념을 이해해야 합니다. Fish Shell에서는 `set` 명령어를 사용하여 변수를 만들 수 있습니다. 다음은 두 개의 문자열을 연결하여 하나의 변수에 저장하는 예시입니다.

```Fish Shell
set str1 "안녕하세요"
set str2 ", 저는 한국어로 쓰여진 블로그 게시물입니다."
set full_str $str1$str2
echo $full_str
```

위 코드를 실행하면 아래와 같은 결과가 나옵니다.

```
안녕하세요, 저는 한국어로 쓰여진 블로그 게시물입니다.
```

또는, `printf` 명령어를 사용하여 문자열을 조합할 수도 있습니다.

```Fish Shell
set name "홍길동"
printf "제 이름은 %s이고, %d세입니다." $name 30
```

```
제 이름은 홍길동이고, 30세입니다.
```

## 심층 분석

위의 예시에서 보듯이, 문자열을 연결하는 방법은 간단합니다. 하지만 더 복잡한 상황에서도 유용하게 사용할 수 있습니다. 예를 들어, 반복문에서 각각 다른 문자열을 조합하여 출력하는 경우에도 쓸 수 있습니다.

또한, Fish Shell에서는 문자열을 날짜나 시간과 같은 다른 형식과 함께 연결하는 기능도 있습니다. 이를 통해 더 다양한 형식의 문자열을 생성할 수 있습니다.

## 참고 자료

- [Fish Shell 공식 문서](https://fishshell.com/docs/current/tutorial.html#tut_strings)
- [Fish Shell 기본 사용법](https://twpower.github.io/63-basic-fish-doc/)
- [Fish Shell로 복잡한 문자열 다루기](https://m.blog.naver.com/kr_genius/221402698196)