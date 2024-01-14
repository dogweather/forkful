---
title:    "Go: 부분 문자열 추출"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/go/extracting-substrings.md"
---

{{< edit_this_page >}}

## 왜

Go 언어에서 substring을 추출하는 것에 대해 관심을 가지는 이유는 간단합니다. 문자열에서 원하는 부분만 추출할 수 있기 때문입니다.

## 어떻게

다음은 Go 언어에서 substring을 추출하는 방법입니다. 코드 블록 안에 있는 코딩 예제와 출력을 참고해주세요.

```Go
// 문자열 변수 선언
str := "안녕하세요, Go 언어입니다."
// 시작 지점과 끝 지점을 지정하여 substring 추출
substr1 := str[3:7] //0부터 시작하여 3~6번째 문자를 추출합니다 (녕하세)
substr2 := str[:3] //맨 처음부터 3번째 문자까지 추출합니다 (안녕하)
substr3 := str[13:] //13번째 문자부터 끝까지 추출합니다 (입니다.)
fmt.Println(substr1)
fmt.Println(substr2)
fmt.Println(substr3)
```

출력:

```
녕하세
안녕하
입니다.
```

## Deep Dive

Go 언어에서 substring을 추출하는 과정은 실제로 매우 간단합니다. 문자열의 인덱스 번호를 활용하여 추출할 부분의 범위를 지정해주면 됩니다. 또한, 문자열의 길이를 넘어갈 경우 자동으로 끝까지 추출되니 이 부분에 유의해야 합니다.

## 참고

- [Go 문자열 함수 및 예제](https://www.tutorialspoint.com/go/go_strings.htm)
- [Go 인덱스 배열 및 슬라이스](https://www.tutorialspoint.com/go/go_indexed_slice.htm)
- [Go 문자열 인덱싱](https://zetawiki.com/wiki/Go_문자열_인덱싱)