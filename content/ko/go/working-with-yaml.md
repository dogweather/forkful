---
title:                "YAML로 작업하기"
html_title:           "Go: YAML로 작업하기"
simple_title:         "YAML로 작업하기"
programming_language: "Go"
category:             "Go"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/go/working-with-yaml.md"
---

{{< edit_this_page >}}

## 왜?

먼저, YAML은 데이터를 사람이 읽기 쉽게 표현하고 다른 시스템 간에 전달하기 위해 사용하는 형식입니다. 이것은 Go 언어에서도 지원되며, YAML을 사용하면 코드를 더 깔끔하고 유지보수하기 쉽게 만들 수 있습니다.

## 어떻게?

다음은 Go 언어에서 YAML을 사용하는 간단한 예제입니다. 우선, YAML 패키지를 가져와야 합니다.

```Go
import “gopkg.in/yaml.v2”
```

이제 YAML 형식의 데이터를 Go 언어의 구조체로 변환하는 방법을 알아보겠습니다. 아래는 YAML 형식의 파일을 읽고, 구조체로 변환한 뒤 출력하는 코드입니다.

```Go
// YAML 형식의 파일을 읽어오는 함수
func readYAML(file string) (myStruct, error) {
    // 구조체 인스턴스 생성
    myData := myStruct{}
    
    // 파일 열기
    f, err := os.Open(file)
    if err != nil {
        return myData, err
    }
    defer f.Close()
    
    // 파일 읽기
    data, err := ioutil.ReadFile(file)
    if err != nil {
        return myData, err
    }
    
    // YAML을 구조체로 변환
    err = yaml.Unmarshal(data, &myData)
    if err != nil {
        return myData, err
    }
    
    // 구조체 반환
    return myData, nil
}

// main 함수에서 호출하기
func main() {
    myData, err := readYAML("myFile.yaml")
    if err != nil {
        fmt.Println(err)
    }
    // 출력
    fmt.Printf("%v", myData)
}
// 출력 결과: {Name: "John" Age: 28}
```

## 더 깊게 들어가기

YAML을 사용하면 구조적인 데이터를 쉽게 표현할 수 있어서 코드를 읽고 이해하기 용이하며, 유지보수하기 쉽습니다. 또한 YAML에서는 주석을 사용할 수 있어서 코드를 더욱 명확하게 만들 수 있습니다. YAML 형식에 대해 더 깊이 알아보고 싶다면 아래의 링크를 참고해보세요.

## 더 알아보기

- [YAML 공식 문서](https://yaml.org/)
- [Go 언어에서의 YAML 사용 예제](https://gobyexample.com/yaml)
- [YAML과 JSON 차이점 비교](https://danielmiessler.com/study/yaml/)
- [YAML 사용 팁](https://dev.to/codeenigma/10-tips-for-writing-better-yaml-1a2)
- [Go 언어 공식 사이트](https://golang.org/)