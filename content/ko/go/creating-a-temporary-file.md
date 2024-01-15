---
title:                "임시 파일 생성"
html_title:           "Go: 임시 파일 생성"
simple_title:         "임시 파일 생성"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/go/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## 왜

임시 파일을 생성하는 작업은 코드 디버깅이나 데이터 처리 등의 작업에서 쉽게 반복적으로 사용되는데, 이를 통해 개발과 작업의 효율성을 크게 높일 수 있습니다.

## 어떻게

임시 파일을 생성하는 가장 간단한 방법은 `ioutil` 패키지의 `TempFile()` 함수를 사용하는 것입니다. 이를 통해 운영 체제가 지정한 디렉토리에 임시 파일을 생성할 수 있으며, 다 사용후 `os.Remove()` 함수를 호출하여 임시 파일을 삭제할 수 있습니다.

```Go
tempFile, err := ioutil.TempFile("", "example") // 빈 문자열은 운영 체제의 기본 임시 디렉토리를 사용함을 의미
if err != nil {
    log.Fatal(err)
}
defer os.Remove(tempFile.Name()) // 생성된 임시 파일 삭제
```

출력 예시:

`/var/folders/54/dv_9hmmj2zjgplvrbd_t2yms2yzjjw/T/example177626838`

임시 파일의 이름은 `example`로 시작하고, 마지막에 고유한 숫자가 추가되어 생성되는 것을 볼 수 있습니다. 여러 작업에서 임시 파일을 생성할 경우, 각각의 고유한 이름을 지정해주어 구분하는 것이 좋습니다.

## 딥 다이브

`TempFile()` 함수의 두 번째 매개변수에는 파일 이름 뒤에 붙는 접두사를 지정할 수 있습니다. 이를 통해 생성되는 임시 파일의 이름을 더욱 명확하게 지정할 수 있습니다. 예를 들어, `example` 대신 `log`를 지정하면 다음과 같은 파일이 생성될 수 있습니다.

`/var/folders/54/dv_9hmmj2zjgplvrbd_t2yms2yzjjw/T/log177626838`

또한, `TempFile()` 함수는 `ioutil.TempDir()` 함수와 함께 사용하여 임시 디렉토리의 이름도 지정할 수 있습니다. 이를 통해 임시 파일을 원하는 디렉토리에 생성할 수 있습니다.

## 참고

- [Go Document: ioutil.TempFile()](https://golang.org/pkg/io/ioutil/#TempFile)
- [Go Document: os.Remove()](https://golang.org/pkg/os/#Remove)
- [Go Document: ioutil.TempDir()](https://golang.org/pkg/io/ioutil/#TempDir)