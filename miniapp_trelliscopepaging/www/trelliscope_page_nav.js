(function() {  // IIF closure ...

  var TrelliscopePageNav = function(el){
    this.el = el;
  };
  (function(){

    this.activeSkip = function(newSkip){
      var activeSkip = $(this.el).find('span.trelliscope-active-skip');
      if (newSkip != null && newSkip != undefined){
        $(activeSkip).html(newSkip);
      }
      return parseInt($(activeSkip).html().replace("x",""));
    };

    this.currentPage = function(page){
      var cp = $(this.el).find('input.trelliscope-page-current');
      if (page != null && page != undefined){
        $(cp).val(page);
        $(cp).trigger("change");
      }
      return parseInt($(cp).val());
    };

    this.pagesTotal = function(){
      return parseInt($(this.el).find('span.trelliscope-pages-total').text());
    };

    this.pageForward = function(){
      var curPage = this.currentPage();
      var nPages = this.pagesTotal();
      var by = this.activeSkip();
      if(curPage + by <= nPages) {
        this.currentPage(curPage + by);
      } else {
        this.pageEnd();
      }
    };

    this.pageBack = function(){
      var curPage = this.currentPage();
      var by = this.activeSkip();
      if(curPage - by >= 1) {
        this.currentPage(curPage - by);
      } else {
        this.pageBegin(1);
      }
    };

    this.pageBegin = function(){
      this.currentPage(1);
    };

    this.pageEnd = function(){
      var nPages = this.pagesTotal();
      this.currentPage(nPages);
    };

  }).call(TrelliscopePageNav.prototype);

  var trelliscopePageNavBinding = new Shiny.InputBinding();
  $.extend(trelliscopePageNavBinding, {
    find: function(scope) {
      return( $(scope).find(".trelliscope-page-nav") );
    },
    initialize: function(el) {
      $(el).find("li a").click(function(e) {
        var pageNav = new TrelliscopePageNav(el);
        pageNav.activeSkip($(this).html());
      });

      $(el).find('button.trelliscope-back-button').click(function() {
        var pageNav = new TrelliscopePageNav(el);
        pageNav.pageBack();
      });
      $(el).find('button.trelliscope-forward-button').click(function() {
        var pageNav = new TrelliscopePageNav(el);
        pageNav.pageForward();
      });
      $(el).find('button.trelliscope-page-end').click(function() {
        var pageNav = new TrelliscopePageNav(el);
        pageNav.pageEnd();
      });
      Shiny.bindAll(el);
    }
  });

  Shiny.inputBindings
    .register(trelliscopePageNavBinding, "shiny.trelliscopePageNavBinding");

}()); // End IIF Closure
