---
title:                "Go: 컴퓨터 프로그래밍: 커맨드 라인 인자 읽기"
programming_language: "Go"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/go/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 왜

Go 프로그래밍 언어를 사용할 때 가장 많이 사용되는 기능 중 하나는 커맨드 라인 인수를 읽는 것입니다. 이 기능을 사용하면 사용자는 프로그램 실행 시 인수를 제공할 수 있으며, 이를 통해 프로그램에 영향을 미칠 수 있습니다. 그래서 우리는 Go에서 커맨드 라인 인수를 읽는 방법에 대해 알아보겠습니다.

## 우리가 어떻게 할까

커맨드 라인 인수를 읽는 것은 아주 간단합니다. 사용자는 프로그램을 실행할 때 함께 제공하는 인수를 받아와서 사용할 수 있습니다. 예를 들어, 다음과 같이 프로그램을 실행하는 경우를 생각해보겠습니다.

```
go run main.go -input hello -output world
```

위 예시에서 `-input`과 `-output`은 각각 인수의 이름이며, `hello`와 `world`는 각 인수에 전달된 값입니다. 이를 Go 코드로 작성하면 다음과 같을 것입니다.

```
func main() {
    input := flag.String("input", "", "input string")
    output := flag.String("output", "", "output string")
    flag.Parse()

    fmt.Println(*input, *output)
}
```

위 코드에서는 `flag` 패키지를 사용하여 인수를 읽고, `flag.String` 메서드를 통해 각 인수의 이름, 기본값, 및 사용법을 설정합니다. 그리고 `flag.Parse()`를 호출하여 인수를 해석해줍니다. 마지막으로 인수 값을 출력하는 예시를 추가했습니다.

## 더 깊이 들어가기

Go에서 인수를 읽는 방법은 `flag` 패키지를 사용하는 것 외에도 `os.Args` 배열을 직접 접근하거나 `os.Args[1:]`를 사용하여 첫 번째 인수부터 끝까지 접근하는 방법도 있습니다. 또한 `flag` 패키지를 사용할 때 더 다양한 타입의 인수를 처리할 수 있으며, `flag` 패키지를 확장하여 사용자 정의 인수도 처리할 수 있습니다. 따라서 `flag` 패키지를 사용하는 방법을 익히는 것이 유용할 것입니다.

## 참고

- [Go 프로그래밍 튜토리얼](https://tour.golang.org/welcome)
- [Go 공식 문서 - flag 패키지](https://golang.org/pkg/flag/)
- [Go 공식 문서 - os 패키지](https://golang.org/pkg/os/)