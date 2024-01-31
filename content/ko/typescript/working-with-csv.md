---
title:                "CSV 파일 다루기"
date:                  2024-01-19
html_title:           "Arduino: CSV 파일 다루기"
simple_title:         "CSV 파일 다루기"

category:             "TypeScript"
tag:                  "Data Formats and Serialization"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/typescript/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why?
CSV(Comma-Separated Values)는 데이터를 저장하고 관리하기 위한 파일 형식입니다. 개발자들은 CSV를 사용해 대량의 데이터를 쉽게 읽고, 쓰고, 교환하기 때문에 선호합니다. 

## How to:
```TypeScript
import * as fs from 'fs';
import * as parse from 'csv-parse/lib/sync';

const csvData = 'id,name,age\n1,John Doe,30\n2,Jane Smith,25';

// CSV 데이터 읽기
const records = parse(csvData, {
  columns: true,
  skip_empty_lines: true
});

console.log(records);

// JSON으로 변환된 데이터를 CSV 파일로 저장하기
const jsonToCsv = (json: object[], filename: string) => {
  const keys = Object.keys(json[0]);
  const csvRows = json.map((row) => keys.map((key) => row[key]).join(','));

  const csvContent = [keys.join(','), ...csvRows].join('\n');

  fs.writeFileSync(filename, csvContent, 'utf8');
};

jsonToCsv(records, 'output.csv');
```

```shell
[ { id: '1', name: 'John Doe', age: '30' },
  { id: '2', name: 'Jane Smith', age: '25' } ]
```

## Deep Dive
CSV 형식은 1970년대부터 사용되며, 단순한 구조 덕분에 다양한 프로그램에서 호환됩니다. JSON이나 XML 같은 형식들도 대안으로 존재하지만, CSV는 낮은 용량과 높은 가독성 덕분에 데이터 교환의 표준 중 하나로 남아있습니다. 노드(Node.js)에서는 'csv-parse'와 같은 라이브러리를 통해 CSV 파일을 쉽게 처리할 수 있습니다.

## See Also
- RFC 4180 (https://tools.ietf.org/html/rfc4180)
- csv-parse documentation (https://csv.js.org/parse/)
- PapaParse (https://www.papaparse.com/) - 브라우져 기반 CSV 파서
- d3-dsv (https://github.com/d3/d3-dsv) - D3 라이브러리의 CSV 파서
