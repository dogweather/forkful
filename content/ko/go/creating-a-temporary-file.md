---
title:                "임시 파일 생성하기"
html_title:           "Go: 임시 파일 생성하기"
simple_title:         "임시 파일 생성하기"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/go/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## 뭐고 왜 하나요?

임시 파일 생성이란 무엇인지 간략히 말하자면, 프로그래머가 프로그램 실행 중에 사용할 수 있는 임시적인 파일을 만드는 것입니다. 이러한 임시 파일은 데이터를 저장하거나 임시적인 작업을 처리하는 등 다양한 용도로 사용될 수 있습니다.

## 하는 법:

```
Go ioutil.TempFile 함수를 사용하면 간단하게 임시 파일을 만들 수 있습니다. 

Go func main() {
    // 아래 코드에서 /tmp 폴더는 실제로 존재하는 경로로 변경해주셔야 합니다.
    dir, err := ioutil.TempDir("/tmp", "example")
    if err != nil {
        log.Fatal(err)
    }
    defer os.RemoveAll(dir)

    // 생성한 임시 폴더 내에 파일 이름을 지정해줍니다.
    f, err := ioutil.TempFile(dir, "foo_*.txt")
    if err != nil {
        log.Fatal(err)
    }

    fmt.Println("임시 파일 경로:", f.Name())

    // 파일에 데이터를 쓰고 저장합니다.
    _, err = f.Write([]byte("임시 데이터"))
    if err != nil {
        log.Fatal(err)
    }

    f.Close()

}
```

#### 출력:

```
임시 파일 경로: /tmp/example/foo_12345.txt
```

## 딥 다이브:

### (1) 역사적인 배경
임시 파일 생성은 초기 운영체제에서부터 사용되던 기능입니다. 하지만 이전에는 자동적으로 삭제되지 않아 디스크에 고스란히 남아있는 경우도 있었습니다. 하지만 최근에는 운영체제 내에서 임시 파일을 관리하는 매커니즘이 발전하여 이러한 문제를 해결할 수 있게 되었습니다.

### (2) 대안
Go에서는 임시 파일 생성을 위해 ```open``` 함수를 사용할 수도 있지만, 이는 보다 복잡한 구현을 필요로 합니다. 따라서 보다 편리한 임시 파일 생성을 위해 ```ioutil.TempFile``` 함수를 사용하는 것이 권장됩니다.

### (3) 구현 세부 사항
Go에서는 임시 파일의 경로와 파일 이름을 랜덤하게 생성하며, 이는 내부적으로 UUID(Universally Unique Identifier)를 사용하여 구현됩니다. 또한 임시 파일 생성 시 지정한 폴더 내에서만 유효하며, 프로그램이 종료되면 자동적으로 삭제됩니다.

## 참고 자료:
- [Go 공식 문서 - ioutil.TempFile](https://golang.org/pkg/io/ioutil/#TempFile)
- [The History of the Temporary File Concept](https://lwn.net/Articles/339188/)