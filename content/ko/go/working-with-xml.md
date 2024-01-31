---
title:                "XML 다루기"
date:                  2024-01-26T04:31:55.523175-07:00
model:                 gpt-4-0125-preview
simple_title:         "XML 다루기"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/go/working-with-xml.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?
XML 작업은 코드를 사용하여 XML 문서를 파싱, 생성 및 조작하는 것을 포함합니다. 프로그래머는 데이터 교환, 구성 파일 및 웹 서비스를 위해 이러한 작업을 수행하는데, XML의 가독성과 광범위한 지원 덕분에 구조화된 데이터를 위한 견고한 선택이 됩니다.

## 방법:
Go에서는 `encoding/xml` 패키지를 사용합니다. XML을 파싱하고 생성해봅시다.
```go
package main

import (
	"encoding/xml"
	"fmt"
	"os"
)

// 구조체는 XML 요소에 매핑됩니다
type Plant struct {
	XMLName xml.Name `xml:"plant"`
	Id      int      `xml:"id,attr"`
	Name    string   `xml:"name"`
	Origin  []string `xml:"origin"`
}

func main() {
	coffee := &Plant{Id: 27, Name: "Coffee"}
	coffee.Origin = []string{"Ethiopia", "Brazil"}

	// 구조체를 XML로 마샬링
	output, err := xml.MarshalIndent(coffee, " ", "  ")
	if err != nil {
		fmt.Printf("Error: %v\n", err)
	}

	os.Stdout.Write([]byte(xml.Header))
	os.Stdout.Write(output)

	// XML을 구조체로 언마샬링
	data := `
<plant id="27">
  <name>Coffee</name>
  <origin>Ethiopia</origin>
  <origin>Brazil</origin>
</plant>
`
	var p Plant
	if err := xml.Unmarshal([]byte(data), &p); err != nil {
		fmt.Printf("Error: %v", err)
		return
	}

	fmt.Printf("\n\nUnmarshaled: %+v", p)
}
```
샘플 출력:
```xml
<?xml version="1.0" encoding="UTF-8"?>
 <plant id="27">
   <name>Coffee</name>
   <origin>Ethiopia</origin>
   <origin>Brazil</origin>
 </plant>

Unmarshaled: {XMLName:{Space: Local:plant} Id:27 Name:Coffee Origin:[Ethiopia Brazil]}
```

## 심층 분석
XML은 90년대 후반부터 존재해왔으며 대규모 전자 출판을 위해 설계되었지만 웹에 빠르게 적용되었습니다. JSON과 같은 대안들이 단순함을 내세워 부상했으나, XML의 스키마와 네임스페이스를 통한 문서 검증은 복잡한 문서에 강력한 기능을 유지합니다. Go에서 `encoding/xml`은 대부분의 작업을 처리하지만, 대형 문서나 스트림 처리의 경우, 더 낮은 수준의 제어와 성능 향상을 위해 `xml.NewDecoder`와 `xml.NewEncoder`를 고려해 볼 수 있습니다.

## 참고 자료
- Go의 `encoding/xml` 패키지: https://pkg.go.dev/encoding/xml
- XML 튜토리얼: https://www.w3schools.com/xml/
- Go 블로그에서의 XML: https://blog.golang.org/xml
- JSON과 XML의 비교: https://www.json.org/xml.html
