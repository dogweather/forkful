---
title:                "Go: 디버그 출력 프린트"
programming_language: "Go"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/go/printing-debug-output.md"
---

{{< edit_this_page >}}

## 왜

디버그 출력을 출력하는 이유는, 프로그래밍 오류를 찾는 데 도움이 됩니다.

## 어떻게 할까?

디버그 출력을 생성하는 가장 간단한 방법은 코드의 특정 지점에 `fmt.Println()` 함수를 호출하는 것입니다. 예를 들어:

```Go
fmt.Println("이것은 디버그 출력입니다.")
```

실행 결과는 다음과 같습니다:

```
이것은 디버그 출력입니다.
```

더 많은 정보를 포함할 수도 있습니다. 예를 들어 변수의 값을 출력하거나, 현재 시간을 출력할 수도 있습니다.

```Go
name := "Jane"
age := 24

fmt.Println("안녕하세요,", name, "나이는", age, "살입니다.")
fmt.Println("현재 시간은", time.Now(), "입니다.")
```

실행 결과는 다음과 같습니다:

```
안녕하세요, Jane 나이는 24 살입니다.
현재 시간은 2022-01-01 12:00:00.000000000 +0900 KST m=+0.000000001 입니다.
```

## 더 깊이 들어가보기

디버그 출력을 더 복잡하게 만들 수도 있습니다. 예를 들어, `if` 문을 사용하여 특정 조건에서만 출력하는 것이 가능합니다.

```Go
num := 10

if num > 5 {
  fmt.Println("숫자가 5보다 큽니다!")
}
```

실행 결과는 다음과 같습니다:

```
숫자가 5보다 큽니다!
```

또한, `for` 루프를 사용하여 여러 줄의 디버그 출력을 만들 수도 있습니다.

```Go
for i := 1; i <= 5; i++ {
  fmt.Println("카운트다운:", i)
}
```

실행 결과는 다음과 같습니다:

```
카운트다운: 1
카운트다운: 2
카운트다운: 3
카운트다운: 4
카운트다운: 5
```

더 복잡한 디버그 출력을 생성하는 방법은 다양하지만, 중요한 점은 디버그 출력을 사용하여 더 효율적으로 프로그래밍 오류를 찾을 수 있다는 점입니다.

## 참조하기

- [https://golang.org/pkg/fmt/](https://golang.org/pkg/fmt/)
- [https://www.geeksforgeeks.org/golang-debugging-with-print-functions/](https://www.geeksforgeeks.org/golang-debugging-with-print-functions/)
- [https://medium.com/code-brewing-company/debugging-using-print-statements-go-lang-b35de29a0105](https://medium.com/code-brewing-company/debugging-using-print-statements-go-lang-b35de29a0105)

## 참고자료

- [https://golang.org/pkg/fmt/](https://golang.org/pkg/fmt/)
- [https://www.geeksforgeeks.org/golang-debugging-with-print-functions/](https://www.geeksforgeeks.org/golang-debugging-with-print-functions/)
- [https://medium.com/code-brewing-company/debugging-using-print-statements-go-lang-b35de29a0105](https://medium.com/code-brewing-company/debugging-using-print-statements-go-lang-b35de29a0105)