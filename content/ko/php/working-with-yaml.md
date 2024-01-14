---
title:                "PHP: yaml 작업하기"
simple_title:         "yaml 작업하기"
programming_language: "PHP"
category:             "PHP"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/php/working-with-yaml.md"
---

{{< edit_this_page >}}

## 왜 YAML을 사용하는가?

YAML은 사람과 컴퓨터 모두 이해하기 쉬운 구조로 데이터를 표현할 수 있는 경량 마크업 언어입니다. 따라서 PHP 개발자로서 YAML을 사용하면 코드를 더욱 간결하고 가독성 높게 작성할 수 있습니다.

## 어떻게 작성하는가?

```PHP
// YAML 데이터 작성
$data = array(
  'name' => 'John Doe',
  'age' => 30,
  'hobbies' => ['reading', 'coding', 'hiking']
);

// YAML 파일에 데이터 저장
yaml_emit_file('data.yaml', $data);

// YAML 파일 로드
$loaded_data = yaml_parse_file('data.yaml');
print_r($loaded_data);
```

```
Array
(
  [name] => John Doe
  [age] => 30
  [hobbies] => Array
  (
    [0] => reading
    [1] => coding
    [2] => hiking
  )
)
```

위의 예시 코드에서는 YAML 데이터를 PHP 배열로 표현하고, `yaml_emit_file()` 함수를 사용하여 해당 배열을 YAML 파일로 저장한 후, `yaml_parse_file()` 함수를 사용하여 YAML 파일을 다시 로드하여 PHP 배열로 변환하는 과정을 보여줍니다.

## 깊이 들어가기

YAML은 들여쓰기에 아주 민감한 언어입니다. 이는 데이터의 구조를 명확하게 보여주고 가독성을 높이는 데 도움을 줍니다. 한 라인에 들어가는 들여쓰기는 공백 2칸, 3칸, 4칸 등 유연하게 지정할 수 있습니다. 또한, YAML은 PHP와 같은 다른 언어와 호환성이 좋기 때문에 데이터를 다양한 형식으로 변환할 수 있습니다.

## 더 알아보기

- [PHP YAML 확장자 문서](https://www.php.net/manual/en/book.yaml.php)
- [YAML 공식 사이트](https://yaml.org/)
- [YAML 튜토리얼](https://rollout.io/blog/yaml-tutorial-everything-you-need-get-started/)