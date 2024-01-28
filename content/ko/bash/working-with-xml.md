---
title:                "XML 다루기"
date:                  2024-01-26T04:27:51.800827-07:00
model:                 gpt-4-0125-preview
simple_title:         "XML 다루기"
programming_language: "Bash"
category:             "Bash"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/bash/working-with-xml.md"
---

{{< edit_this_page >}}

## 무엇과 왜?
XML을 다룬다는 것은 Extensible Markup Language 형식의 데이터를 파싱하고, 추출하며, 조작하는 것을 의미합니다. 프로그래머들은 설정, API 등을 위한 광범위한 데이터 교환 형식으로 사용되기 때문에 XML과 씨름합니다.

## 방법:
Bash에서 XML을 파싱하는 방법입니다. 도구? xmllint와 xmlstarlet입니다. XML 요소를 반복하기? 당연합니다. 샘플 출력과 함께 예시:

```bash
# xmlstarlet이 설치되어 있다고 가정
# 설치 방법: apt-get install xmlstarlet

# XML 내용 파싱
cat <<EOF > sample.xml
<fruits>
  <fruit name="Apple"/>
  <fruit name="Banana"/>
</fruits>
EOF

# xmlstarlet을 사용하여 이름 추출
xmlstarlet sel -t -m "//fruit" -v "@name" -n sample.xml

# 출력 결과는 다음과 같아야 합니다:
# Apple
# Banana
```

## 깊이 들어가기
90년대에 XML이 SGML에 비해 더 단순한 대안으로, HTML보다 더 구조화된 것으로 나타났습니다. 지금은 JSON, YAML 등의 새로운 친구들이 등장했습니다. 그러나 XML은 특히 설정과 SOAP 기반 웹 서비스에서 여전히 중요합니다.

도구 측면에서 xmllint는 XML 검증, xpath 쿼리에 편리합니다. xmlstarlet은 XML 쿼리, 편집, 검증, 변환과 같은 일에 있어 스위스 군용 칼입니다. bash 스크립트에서 이들은 XML 작업을 위한 슈퍼히어로입니다.

내부적으로, xmllint는 libxml2를 사용합니다 – C로 작성된 XML 파서입니다. 빠르지만, 오류 메시지는? 알아보기 어렵습니다. 그리고 xmlstarlet? 재귀 템플릿과 EXSLT 지원을 포함하고 있습니다. 마음을 사로잡지만 강력합니다.

## 참고 자료
- [xmlsoft.org](http://xmlsoft.org/): Libxml2 및 xmllint에 관한 자료.
- [Stack Overflow](https://stackoverflow.com/questions/tagged/xml+bash): 실제 문제와 해결책.
- [W3Schools XML 튜토리얼](https://www.w3schools.com/xml/): XML의 기초.
