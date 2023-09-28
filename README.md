# alternet
Package to convert network diagrams between different formats.

<br>

## Supported formats

* Kumu
* Decision Explorer
* Stella

<br>

## Supported characteristics

* Node name
* Node position
* Node type
* Edge polarity
* Styles for node types

<br>

## Getting Started

Once cloned, open up the project, and load the package with:

``` r
devtools::load_all()
```

Documentation is available for the package functions, e.g.

``` r
?import_from_kumu_json
```

<br>

### Example

``` r
devtools::load_all()

network = import_from_kumu_json("inst/extdata/example_network.json")

network %>% export_to_decision_explorer()
```

<br>

### Getting data from original software

The import functions work directly on save files produced by Stella (".stmx") and Decision Explorer (".demx"). You can also use the files produced using the Decision Explorer export facility ("mdx").

For Stella, you can either import from a causal loop diagram, in which case the variables will be interpreted as nodes, or from a stock and flow model, in which case the modules will be interpreted as nodes. See the documentation for import_from_stella_xml.

For Kumu, see the documentation on exporting data as json, click the download icon in the lower right corner of the map and choose "Export to JSON".

<br>

### Using exported data in the new software

For Stella, simply open the new stmx file in Stella.

For Kumu, see import instructions [on the Kumu website]("https://docs.kumu.io/guides/import/blueprints#import-a-json-file").

For Decision Explorer, create a new, blank model, go to "File -> Import" and choose the file you want to import.
