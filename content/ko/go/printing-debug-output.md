---
title:    "Go: 디버그 출력하기"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/go/printing-debug-output.md"
---

{{< edit_this_page >}}

## 왜 디버그 출력을 사용하는가?

프로그래밍을 할 때, 버그를 찾고 해결하기 위해 디버그 출력은 매우 유용합니다. 코드의 실행 중간에 변수의 값을 확인하거나 코드의 특정 부분이 실행되는지 확인할 수 있기 때문입니다. 이를 통해 코드를 더 간단하게 디버깅할 수 있습니다.

## 디버그 출력하는 방법

디버그 출력은 Go에서 매우 쉽게 할 수 있습니다. 메시지를 출력하고 싶은 부분에서 `fmt.Println()`을 사용하면 됩니다. `fmt` 패키지에서 제공하는 출력 함수는 다양한 데이터 타입을 지원합니다. 예를 들어, 숫자를 출력하려면 `%d`를 사용하고, 문자열을 출력하려면 `%s`를 사용하면 됩니다.

```Go
// 정수 출력 예시
age := 28
fmt.Println("나의 나이는 %d살입니다.", age)

// 문자열 출력 예시
name := "John"
fmt.Println("내 이름은 %s입니다.", name)
```

코드를 실행하면 다음과 같은 출력 결과가 나옵니다.

```
나의 나이는 28살입니다.
내 이름은 John입니다.
```

## 딥 다이브

Go에서는 디버그 출력을 위해 `fmt` 패키지 이외에도 `log` 패키지를 제공합니다. `log` 패키지를 사용하면 메시지 이외에도 시간, 모듈 정보 등의 추가 정보를 출력할 수 있습니다. 또한, `log` 패키지는 로그의 레벨을 지정할 수 있어서 중요도에 따라 다른 메시지를 출력할 수도 있습니다.

```Go
// 로그 레벨 설정 예시
log.SetPrefix("MyApp: ")
log.SetFlags(log.Ldate | log.Lmicroseconds)
log.SetOutput(os.Stdout)
log.Println("앱이 시작되었습니다.")

// 로그 출력 예시
log.Println("정보 로그")
log.Println("경고 로그")
log.Println("오류 로그")
```

출력 결과는 다음과 같습니다.

```
MyApp: 2021/01/01 01:23:45.678910 앱이 시작되었습니다.
MyApp: 2021/01/01 01:23:45.678910 정보 로그
MyApp: 2021/01/01 01:23:45.678910 경고 로그
MyApp: 2021/01/01 01:23:45.678910 오류 로그
```

## 관련 자료

- [Go 디버깅을 위한 디버그 출력의 사용 예시](https://pkg.go.dev/log#example-Println)
- [fmt 패키지 문서](https://pkg.go.dev/fmt)
- [log 패키지 문서](https://pkg.go.dev/log)

## 더 많은 자료

- [Go 언어 공식 문서 (한국어)](https://go-tour-kr.appspot.com/list)
- [Go 언어 공식 GitHub 저장소](https://github.com/golang/go)
- [Go 언어로 만든 유용한 패키지들 모음](https://awesome-go.com/)