---
title:    "Bash: 텍스트 검색 및 교체"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/bash/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## 왜
텍스트를 검색하고 대체하는 것에 참여하는 이유는 프로그래밍 언어에서 가장 기본적인 작업 중 하나이기 때문입니다.

## 어떻게
검색 및 대체 작업을 수행하는 Bash 예제와 해당 예제의 샘플 출력을 "```Bash ... ```" 코드 블록 내에서 제공합니다.

```Bash
# 파일 내의 특정 문자열 모두 찾아 대체하기
sed -i 's/원래문자열/새로운문자열/g' 파일명

# 사용자 입력을 받아 대소문자를 모두 대체하기
read -p "대체할 문자열을 입력하세요: " 대체문자열
read -p "대체될 문자열을 입력하세요: " 새문자열
sed -i 's/$대체문자열/$새문자열/gI' 파일명 
```

## 딥 다이브
텍스트 검색 및 대체 작업은 RegEx (정규 표현식)을 사용하여 더욱 유연하고 정교하게 수행할 수 있습니다. 이를 통해 더 복잡한 패턴을 찾아 대처할 수 있습니다. 또한, 검색 및 대체 작업을 자동화하는 방법에 대해 배울 수도 있습니다.

## 참고
"대체"와 "검색" 기능에 대해 자세히 알아보려면 다음 링크를 방문해보세요.

- [Bash에서 sed 사용하기](https://www.lesstif.com/pages/viewpage.action?pageId=20776485)
- [Regexr: RegEx 표현식 테스트하기](https://regexr.com/)
- [생활코딩: 정규표현식 in Bash](https://opentutorials.org/module/430/3581)

## 참고자료
- [Bash script를 이용하여 일괄변경 file 변경하기](https://thrillfighter.tistory.com/107)
- [bash script - 문자열 치환](https://github.com/minsoo9506/TIL/blob/master/%EB%B0%B1%EB%82%AD%EC%9D%B4%EC%A7%80%ED%91%9C%ED%98%84.md)

- [bash에서 문자열 검색 및 대체하기](https://goddaehee.tistory.com/157)