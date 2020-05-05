# Release notes

**2.1.0**: Now handling Commentary relations very literally, according to the Cite Relations in the data; no longer trying to be clever.

**2.0.1**: Fixed bug where ROIs mapped to images would appear N^2 times in the sidebar of the image-view.

**2.0.0**: Update DSE library (a breaking change). Incorporating Extended String Types for including Markdown and geo-spatial references in Object properties. Improved timing for asynchronous image loading from a paramter URN.

**1.10.0**: Using CiteJson 3.0.0.

**1.10.0**: Using OHCO2 10.12.5 and xCite 4.0.2.

**1.9.3**: Commentary relations are transitive, at least as far as the Browse Texts tab is concerned.

**1.9.2**: Fixed bug when including a URN as a request-parameter, when the URN is not in the data. Request-parameter URNs will automatically be loaded into the URN-text input field (CTS URNs only for 1.9.2).

**1.9.1**: Matching text to commentary even when the relation is at the work-level.

**1.9.0**: Better handling of links: allows opening URNs in a new window, with a new instance of the App.

**1.8.0**: Adding link to look up any object, or Cite2Urn propert-value in Relations.

**1.7.2**: Minor bug reporting number of objects in a collection.

**1.7.1**: Fixed "race condition" involving image-rois.

**1.7.0**: Accepting a `urn=` request parameter, CTS or CITE2 URN.

**1.6.0**: Commentary implemented.

**1.5.0**: Relations implemented.

**1.4.0**: DSE Data Model implemented. There remain issues (see GitHub) with asynchronous loading of DSE data. 

**1.3.0**: Rebuild CiteCollection handling to push much more work back onto the server.

**1.2.0**: Images.

**1.1.0**: Collection browsing and searching.

**1.0.0**: All text functions working.

**0.1.1**: Ngrams and String Searching implemented.

**0.1.0**: Text Browsing implemented.

**0.0.1**: initial development.
