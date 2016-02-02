class ShopifyAPIClient
  
    attr_reader :shop_id

    # base url
    SECRET_API_KEY = "asdasdasQWRESFSDFVSDFASDFSADFASDF123123ASDASD$%$%"
    BASE_URL = "https://www.shopify.this.is.a.sample.com/"
  
    def initialize(password_ini = '')
        # use universal header
        @params = Hash.new
        @params[:basic_auth] = {username: SECRET_API_KEY, password: password_ini}
    end
   
    def set_shop_id(shop_Id)
        # set shop id parameter
        @shop_id = shop_Id
        @shop_url = BASE_URL + shop_Id.to_s
    end
  
    # request builder, to json
    def get_json(url)
        if shop_id_isset
            begin
                result_json = some_http_library.get(url, params)
            rescue
                # try, catch and rescue
            end
            return JSON.parse(result_json)
        end
    end
  
    # validation is set of shop_id
    def shop_id_isset 
        if @shop_id == nil  
            raise "\"shop_id\" is missing\n" +
            "Use method: set_shop_id to initialize \"shop_id\"" 
        end
        return true
    end
  
    # simplified the construction of requests
    def orders
        json_url = @shop_url + "/orders"
        return get_json(json_url)
    end
  
    def products
        json_url = @shop_url + "/products"
        return get_json(json_url)
    end
  
    def product(id)
        json_url = @shop_url + "/products/#{id}"
        return get_json(json_url)
    end
  
    # private members
    private :get_json, :shop_id_isset
end